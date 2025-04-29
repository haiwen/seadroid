package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.ListenableWorker;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.notification.FileBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.util.UUID;


/**
 * Manually select file upload
 *
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class FileUploadWorker extends BaseUploadWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(FileUploadWorker.class.getSimpleName().getBytes());

    private final FileBackupNotificationHelper notificationManager;

    public FileUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
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
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        int totalPendingCount = GlobalTransferCacheList.FILE_UPLOAD_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            return returnSuccess();
        }

        showNotification();

        // This exception is a type of interruptible program, and a normal exception does not interrupt the transfer task
        // see BaseUploadWorker#isInterrupt()
        String interruptibleExceptionMsg = null;

        while (true) {
            SLogs.d("start upload file worker");
            if (isStopped()) {
                break;
            }

            TransferModel transferModel = GlobalTransferCacheList.FILE_UPLOAD_QUEUE.pick();
            if (transferModel == null) {
                break;
            }

            //uploadModel field data (related_account/repo_id/repo_name) has been filled

            try {
                try {
                    transfer(account, transferModel);

                } catch (Exception e) {
                    SeafException seafException = ExceptionUtils.parseByThrowable(e);
                    //Is there an interruption in the transmission in some cases?
                    boolean isInterrupt = isInterrupt(seafException);
                    if (isInterrupt) {
                        SLogs.e("An exception occurred and the transmission has been interrupted");
                        notifyError(seafException);

                        // notice this, see BaseUploadWorker#isInterrupt()
                        throw e;
                    } else {
                        SLogs.e("An exception occurred and the next transfer will continue");
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

        showToast(R.string.upload_finished);
        SLogs.e("file upload: complete");

        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, interruptibleExceptionMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(TransferDataSource.FILE_BACKUP, TransferEvent.EVENT_TRANSFER_FINISH, b);
        return Result.success();
    }


    protected Result returnSuccess() {
        sendWorkerEvent(TransferDataSource.FILE_BACKUP, TransferEvent.EVENT_TRANSFER_FINISH);
        return Result.success();
    }
}
