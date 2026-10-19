package com.seafile.seadroid2.framework.worker.periodic;

import static android.app.PendingIntent.FLAG_IMMUTABLE;

import android.app.ForegroundServiceStartNotAllowedException;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.core.app.NotificationCompat;
import androidx.work.ForegroundInfo;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.service.scan.FolderScanHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.main.MainActivity;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.UUID;

public class FolderBackupPeriodicScanStarter extends com.seafile.seadroid2.framework.worker.BaseWorker {
    public static final String TAG = "FolderBackupScanStarter";
    public static final UUID UID = UUID.nameUUIDFromBytes(FolderBackupPeriodicScanStarter.class.getSimpleName().getBytes());
    private final FolderBackupScanNotificationHelper notificationManager;

    public FolderBackupPeriodicScanStarter(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupScanNotificationHelper(context);
    }

    // Legacy notification signature only; periodic work does not own new business notifications.
    private void showNotification() { }

    private static boolean canExc() {
        boolean isRunning = BackupThreadExecutor.getInstance().isFolderBackupRunning();
        return !isRunning;
    }

    @NonNull
    @Override
    public Result doWork() {

        SLogs.e(TAG, "文件夹扫描 Worker 启动");

        if (isTransferStopped()) return Result.retry();

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SLogs.d(TAG, "The folder scan task was not started, because the switch is off");
            return returnSuccess();
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SLogs.d(TAG, "The folder scan task was not started, because the repo is not selected");
            return returnSuccess();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SLogs.d(TAG, "The folder scan task was not started, because the folder path is not selected");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SLogs.e(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        showNotification();

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        //scan
        int count = FolderScanHelper.onlyTraverseBackupPathFileCount(backupPaths, account, repoConfig);
        SLogs.d(TAG, "periodic folder scan task complete", "count: " + count);

        // Scan-only: no transfer result until the separate upload Worker actually runs.

        return returnSuccess();
    }

    protected Result returnSuccess() {
        return Result.success();
    }


    // Legacy independent scan-result publishing intentionally removed.
}
