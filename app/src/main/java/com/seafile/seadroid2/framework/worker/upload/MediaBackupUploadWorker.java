package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.nfc.Tag;
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;

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
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.AlbumBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.UUID;

/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class MediaBackupUploadWorker extends BaseUploadWorker {
    private final String TAG = "MediaBackupUploadWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(MediaBackupUploadWorker.class.getSimpleName().getBytes());

    private final AlbumBackupNotificationHelper notificationManager;

    public MediaBackupUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new AlbumBackupNotificationHelper(context);
    }

    @Override
    public BaseTransferNotificationHelper getNotification() {
        return notificationManager;
    }

    private void showNotification() {
        String title = getApplicationContext().getString(R.string.settings_camera_upload_info_title);
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

    @NonNull
    @Override
    public ListenableWorker.Result doWork() {
        SLogs.d(TAG, "doWork()", "started execution");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }


        boolean canContinue = can();
        if (!canContinue) {
            SLogs.d(TAG, "doWork()", "settings missing config or not turned on");
            return returnSuccess();
        }

        //
        int totalPendingCount = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SLogs.d(TAG, "doWork()", "backup queue is empty");
            return returnSuccess();
        }

        showNotification();
        //send a upload event
//        sendActionEvent(TransferDataSource.ALBUM_BACKUP, TransferEvent.EVENT_UPLOADING);

        // This exception is a type of interruptible program, and a normal exception does not interrupt the transfer task
        // see BaseUploadWorker#isInterrupt()
        String interruptibleExceptionMsg = null;

        while (true) {
            if (isStopped()) {
                break;
            }

            try {
                TransferModel transferModel = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.pick();
                if (transferModel == null) {
                    break;
                }

                transferModel.related_account = account.getSignature();
                transferModel.repo_id = repoConfig.getRepoId();
                transferModel.repo_name = repoConfig.getRepoName();

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
        SLogs.d(TAG, "doWork()", "complete");
        //
        //
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_RESULT, interruptibleExceptionMsg);
        b.putInt(TransferWorker.KEY_TRANSFER_COUNT, totalPendingCount);
        sendWorkerEvent(TransferDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_FINISH, b);
        return Result.success();
    }

    protected Result returnSuccess() {
        sendWorkerEvent(TransferDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_FINISH);
        return Result.success();
    }

    private RepoConfig repoConfig;

    private boolean can() {
        boolean isTurnOn = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            return false;
        }

        repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null) {
            return false;
        }

        if (TextUtils.isEmpty(repoConfig.getRepoId())) {
            return false;
        }

        return true;
    }
}
