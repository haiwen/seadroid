package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.List;
import java.util.UUID;


/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class FolderBackupUploadWorker extends BaseUploadWorker {
    private final String TAG = "FolderBackupUploadWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(FolderBackupUploadWorker.class.getSimpleName().getBytes());

    private final FolderBackupNotificationHelper notificationManager;

    public FolderBackupUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
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
        String title = getApplicationContext().getString(R.string.settings_folder_backup_info_title);
        String subTitle = getApplicationContext().getString(R.string.upload_started);

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
        SLogs.d(TAG, "start()", "started execution");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean canContinue = can();
        if (!canContinue) {
            SLogs.d(TAG, "start()", "settings missing config or not turned on");
            return returnSuccess();
        }


        //
        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SLogs.d(TAG, "start()", "backup queue is empty");
            return returnSuccess();
        }

        //
        showNotification();
        //send a upload event
//        sendActionEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_UPLOADING);

        // This exception is a type of interruptible program, and a normal exception does not interrupt the transfer task
        // see BaseUploadWorker#isInterrupt()
        String interruptibleExceptionMsg = null;

        while (true) {
            if (isStopped()) {
                break;
            }

            try {
                TransferModel transferModel = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.pick();
                if (transferModel == null) {
                    break;
                }

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
        SLogs.d(TAG, "start()", "complete");

        //
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, interruptibleExceptionMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE, b);
        return Result.success();
    }


    private RepoConfig repoConfig;

    protected Result returnSuccess() {
        sendWorkerEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return Result.success();
    }

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
