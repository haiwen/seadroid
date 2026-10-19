package com.seafile.seadroid2.framework.worker.periodic;

import static android.app.PendingIntent.FLAG_IMMUTABLE;

import android.app.ForegroundServiceStartNotAllowedException;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.core.app.NotificationCompat;
import androidx.work.ForegroundInfo;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.AlbumBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.service.scan.AlbumScanHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.main.MainActivity;

import java.util.UUID;

public class AlbumBackupPeriodicScanStarter extends com.seafile.seadroid2.framework.worker.BaseWorker {
    public static final String TAG = "AlbumBackupPeriodicScanStarter";
    public static final UUID UID = UUID.nameUUIDFromBytes(AlbumBackupPeriodicScanStarter.class.getSimpleName().getBytes());
    private final AlbumBackupScanNotificationHelper notificationManager;

    public AlbumBackupPeriodicScanStarter(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new AlbumBackupScanNotificationHelper(context);

    }

    private static boolean canExc() {
        boolean isRunning = BackupThreadExecutor.getInstance().isAlbumBackupRunning();
        return !isRunning;
    }

    // Legacy notification signature only; periodic work does not own new business notifications.
    private void showNotification() { }

    @NonNull
    @Override
    public Result doWork() {

        SLogs.d(TAG, "doWork()", "started execution");
        if (isTransferStopped()) return Result.retry();

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the switch is off");
            return returnSuccess();
        }

        Account backupAccount = CameraUploadManager.getInstance().getCameraAccount();
        if (backupAccount == null) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the backup account is null");
            return returnSuccess();
        }

        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || TextUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the repoConfig is null");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.e(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        showNotification();

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            AlbumBackupSharePreferenceHelper.resetLastScanTime();
        }

        boolean hasResult = AlbumScanHelper.readMediaResult(getApplicationContext(), account, repoConfig);
        SLogs.d(TAG, "periodic album scan task complete", "hasResult: " + hasResult);

        // Scan-only: do not claim upload completion or publish an empty-transfer summary.

        //
        return returnSuccess();
    }

    protected Result returnSuccess() {
        return Result.success();
    }

    // Legacy independent scan-result publishing intentionally removed.

}
