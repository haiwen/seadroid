package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.List;
import java.util.UUID;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class UploadFolderFileAutomaticallyWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(UploadFolderFileAutomaticallyWorker.class.getSimpleName().getBytes());

    private final FolderBackupNotificationHelper notificationManager;

    public UploadFolderFileAutomaticallyWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupNotificationHelper(context);
    }

    @Override
    public BaseTransferNotificationHelper getNotification() {
        return notificationManager;
    }

    @NonNull
    @Override
    public Result doWork() {
        return start();
    }

    @Override
    public void onStopped() {
        super.onStopped();

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            SLogs.e("Folder backup stopped, reason：" + getStopReason());
        }
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


    private Result start() {
//        notificationManager.cancel();
        SLogs.d("start upload file worker");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        boolean canContinue = can();
        if (!canContinue) {
            return Result.success();
        }

        if (repoConfig == null) {
            return Result.success();
        }

        //get total count: WAITING, IN_PROGRESS, FAILED
        long totalPendingCount = getCurrentPendingCount(account, TransferDataSource.FOLDER_BACKUP);
        if (totalPendingCount <= 0) {
            return Result.success(getOutputData(null));
        }

        //
        showNotification();

        // This exception is a type of interruptible program, and a normal exception does not interrupt the transfer task
        // see BaseUploadWorker#isInterrupt()
        String interruptibleExceptionMsg = null;
        boolean isFirst = true;

        while (true) {
            if (isStopped()) {
                break;
            }

            List<FileTransferEntity> transferList = getList(isFirst, account);
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
                    // Upload to the default repo
                    fileTransferEntity.repo_id = repoConfig.getRepoId();
                    fileTransferEntity.repo_name = repoConfig.getRepoName();
                    fileTransferEntity.result = null;// reset result

                    try {
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
        long pendingCount = getCurrentPendingCount(account, TransferDataSource.FOLDER_BACKUP);
        if (pendingCount == 0) {
            ToastUtils.showLong(R.string.upload_finished);
        }

        SLogs.e("UploadFolderFileAutomaticallyWorker all task run");

        Data outputData = getOutputData(interruptibleExceptionMsg);
        return Result.success(outputData);
    }

    private Data getOutputData(String exceptionMsg) {
        return new Data.Builder()
                .putString(TransferWorker.KEY_DATA_SOURCE, TransferDataSource.FOLDER_BACKUP.name())
                .putString(TransferWorker.KEY_DATA_STATUS, TransferEvent.EVENT_FINISH)
                .putString(TransferWorker.KEY_DATA_RESULT, exceptionMsg)
                .build();
    }

    private List<FileTransferEntity> getList(boolean isFirst, Account account) {
        List<FileTransferEntity> transferList;
        if (isFirst) {
            //get all: FAILED
            transferList = AppDatabase.getInstance()
                    .fileTransferDAO()
                    .getOneFailedPendingTransferSync(
                            account.getSignature(),
                            TransferDataSource.FOLDER_BACKUP
                    );
        } else {
            //get one: WAITING, IN_PROGRESS
            transferList = AppDatabase.getInstance()
                    .fileTransferDAO()
                    .getOnePendingTransferSync(
                            account.getSignature(),
                            TransferDataSource.FOLDER_BACKUP
                    );
        }

        return transferList;
    }

    private RepoConfig repoConfig;

    private boolean can() {
        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            return false;
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            return false;
        }

        repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null) {
            return false;
        }

        return true;
    }

}
