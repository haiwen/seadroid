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

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.notification.FileUploadNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

import java.util.UUID;


/**
 * Manually select file upload
 */
@Deprecated
public class FileUploadWorker extends BaseUploadWorker {
    private static final String TAG = "FileUploadWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(TAG.getBytes());

    private final FileUploadNotificationHelper notificationManager;

    public FileUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FileUploadNotificationHelper(context);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.MANUAL_FILE_UPLOAD;
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
        SLogs.d(TAG, "start()", "started execution");

        int totalPendingCount = GlobalTransferCacheList.FILE_UPLOAD_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            return returnSuccess();
        }

        showNotification();

        // This exception is a type of interruptible program, and a normal exception does not interrupt the transfer task
        // see BaseUploadWorker#isInterrupt()
        String interruptibleExceptionMsg = null;

        while (true) {
            SLogs.d(TAG, "start()", "start upload file worker");
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
                    Account specialAccount = SupportAccountManager.getInstance().getSpecialAccount(transferModel.related_account);
                    if (specialAccount == null) {
                        return returnSuccess();
                    }

                    transfer(specialAccount, transferModel);

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
        SLogs.d(TAG, "start()", "complete");

        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, interruptibleExceptionMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
//        send(FeatureDataSource.MANUAL_FILE_UPLOAD, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);
        return Result.success();
    }


    protected Result returnSuccess() {
        send(FeatureDataSource.MANUAL_FILE_UPLOAD, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return Result.success();
    }
}
