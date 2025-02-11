package com.seafile.seadroid2.framework.worker.upload;

import android.app.ActivityManager;
import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.ListenableWorker;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.notification.FileBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.util.List;
import java.util.UUID;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class UploadFileManuallyWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(UploadFileManuallyWorker.class.getSimpleName().getBytes());

    private final FileBackupNotificationHelper notificationManager;

    public UploadFileManuallyWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FileBackupNotificationHelper(context);
    }

    @Override
    public BaseTransferNotificationHelper getNotification() {
        return notificationManager;
    }

    @NonNull
    @Override
    public ListenableWorker.Result doWork() {
        return start();
    }

    private void showNotification() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
                showForegroundAsync(foregroundInfo);
            } catch (ForegroundServiceStartNotAllowedException e) {
                SLogs.e(e.getMessage());
            }
        } else {
            ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
            showForegroundAsync(foregroundInfo);
        }
    }

    /**
     * The task here may not be from the current account
     */
    private ListenableWorker.Result start() {

        //get total count: WAITING, IN_PROGRESS, FAILED
        long totalPendingCount = getCurrentPendingCountForAllAccount(TransferDataSource.FILE_BACKUP);
        if (totalPendingCount <= 0) {
            return Result.success(getOutputData(null));
        }

        showNotification();

        // This exception is a type of interruptible program, and a normal exception does not interrupt the transfer task
        // see BaseUploadWorker#isInterrupt()
        String interruptibleExceptionMsg = null;
        boolean isFirst = true;

        while (true) {
            SLogs.d("start upload file worker");
            if (isStopped()) {
                return Result.success();
            }

            List<FileTransferEntity> transferList = getList(isFirst);
            if (isFirst) {
                isFirst = false;

                if (CollectionUtils.isEmpty(transferList)) {
                    continue;
                }
            } else if (CollectionUtils.isEmpty(transferList)) {
                break;
            }

            try {
                for (FileTransferEntity fileTransferEntity : transferList) {
                    try {
                        String relatedAccount = fileTransferEntity.related_account;
                        Account account = SupportAccountManager.getInstance().getSpecialAccount(relatedAccount);

                        //transfer
                        transfer(account, fileTransferEntity, totalPendingCount);

                    } catch (Exception e) {
                        SeafException seafException = ExceptionUtils.getExceptionByThrowable(e);
                        //Is there an interruption in the transmission in some cases?
                        boolean isInterrupt = isInterrupt(seafException);
                        if (isInterrupt) {
                            SLogs.e("上传文件时发生了异常，已中断传输");
                            notifyError(seafException);

                            // notice this, see BaseUploadWorker#isInterrupt()
                            throw e;
                        } else {
                            SLogs.e("上传文件时发生了异常，继续下一个传输");
                        }

                    }
                }
            } catch (Exception e) {
                SLogs.e("upload file file failed: ", e);
                interruptibleExceptionMsg = e.getMessage();

                break;
            }
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (getStopReason() >= WorkInfo.STOP_REASON_CANCELLED_BY_APP) {
                interruptibleExceptionMsg = SeafException.USER_CANCELLED_EXCEPTION.getMessage();
            }
        }

        //get FAILED count
        long pendingCount = getCurrentPendingCountForAllAccount(TransferDataSource.FILE_BACKUP);
        if (pendingCount == 0) {
            ToastUtils.showLong(R.string.upload_finished);
        }

        SLogs.e("UploadFileManuallyWorker all task run");
        return ListenableWorker.Result.success(getOutputData(interruptibleExceptionMsg));
    }

    private List<FileTransferEntity> getList(boolean isFirst) {
        List<FileTransferEntity> transferList;
        if (isFirst) {
            //get all: FAILED
            transferList = AppDatabase.getInstance()
                    .fileTransferDAO()
                    .getOnePendingFailedTransferAllAccountSync(TransferDataSource.FILE_BACKUP);
        } else {
            //get one: WAITING, IN_PROGRESS
            transferList = AppDatabase.getInstance()
                    .fileTransferDAO()
                    .getOnePendingTransferAllAccountSync(TransferDataSource.FILE_BACKUP);
        }

        return transferList;
    }

    private Data getOutputData(String exceptionMsg) {
        return new Data.Builder()
                .putString(TransferWorker.KEY_DATA_SOURCE, TransferDataSource.FILE_BACKUP.name())
                .putString(TransferWorker.KEY_DATA_STATUS, TransferEvent.EVENT_FINISH)
                .putString(TransferWorker.KEY_DATA_RESULT, exceptionMsg)
                .build();
    }

    /**
     * If have already downloaded the record,
     * it means that the transfer mode of this file is synchronous mode,
     * not just manual upload, and there is no need to delete the local file.
     */
    private boolean checkAndDeleteIfNecessary(FileTransferEntity uploadEntity) {

        //
        List<FileTransferEntity> files = AppDatabase.getInstance().fileTransferDAO().getListByFullPathSync(uploadEntity.repo_id, TransferDataSource.DOWNLOAD, uploadEntity.target_path);
        return CollectionUtils.isEmpty(files);
    }

}
