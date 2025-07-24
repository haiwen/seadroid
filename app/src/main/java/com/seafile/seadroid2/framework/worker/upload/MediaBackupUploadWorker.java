package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.ListenableWorker;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.AlbumBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.UUID;

/**
 * Worker Tag:
 */
public class MediaBackupUploadWorker extends BaseUploadWorker {
    private static final String TAG = "MediaBackupUploadWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(TAG.getBytes());

    private final AlbumBackupNotificationHelper notificationManager;

    public MediaBackupUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new AlbumBackupNotificationHelper(context);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.ALBUM_BACKUP;
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
//send a start event
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_START);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            SafeLogs.d(TAG, "account is null");
            return returnSuccess();
        }
        boolean isTurnOn = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "backup switch is off");
            return returnSuccess();
        }

        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null) {
            SafeLogs.d(TAG, "repo config is null");
            return returnSuccess();
        }

        if (TextUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "repo id is empty");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.e(TAG, "data plan is not allowed", "current network type", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type", NetworkUtils.getNetworkType().name());
        }


        //
        int totalPendingCount = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SafeLogs.e(TAG, "backup queue is empty");
            return returnSuccess();
        }

        SafeLogs.e(TAG, "pending count: " + totalPendingCount);

        SeafException resultException = SeafException.SUCCESS;
        SeafException interruptException = SeafException.SUCCESS;
        SeafException lastException = SeafException.SUCCESS;

        while (true) {
            TransferModel transferModel = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.pick();
            if (transferModel == null) {
                break;
            }

            transferModel.related_account = account.getSignature();
            transferModel.repo_id = repoConfig.getRepoId();
            transferModel.repo_name = repoConfig.getRepoName();

            try {

                transfer(account, transferModel);

            } catch (SeafException seafException) {
                // In some cases, the transmission needs to be interrupted
                boolean isInterrupt = isInterrupt(seafException);
                if (isInterrupt) {
                    SafeLogs.e("An exception occurred and the transmission has been interrupted");
                    notifyError(seafException);

                    interruptException = seafException;
                    break;
                } else {
                    lastException = seafException;
                    SafeLogs.e("An exception occurred and the next transfer will continue");
                }
            }

        }

        if (interruptException != SeafException.SUCCESS) {
            resultException = interruptException;
            SafeLogs.d(TAG, "all completed", "error msg:[interruptException]: " + resultException.getMessage());
        } else if (totalPendingCount == 1 && lastException != SeafException.SUCCESS) {
            resultException = lastException;
            SafeLogs.d(TAG, "all completed", "error msg:[lastException]: " + resultException.getMessage());
        } else {
            SafeLogs.d(TAG, "all completed");
        }

        String errorMsg = null;
        if (resultException != SeafException.SUCCESS) {
            errorMsg = resultException.getMessage();
            Toasts.show(R.string.backup_failed);
        }else{
            Toasts.show(R.string.backup_completed);
        }

        sendCompleteEvent(FeatureDataSource.ALBUM_BACKUP, errorMsg, totalPendingCount);
        return Result.success();
    }

    protected Result returnSuccess() {
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return Result.success();
    }
}
