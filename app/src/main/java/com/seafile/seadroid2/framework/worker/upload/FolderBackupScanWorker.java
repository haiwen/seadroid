package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.service.scan.FolderScanHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.UUID;

public class FolderBackupScanWorker extends BaseScanWorker {
    public static final String TAG = "FolderBackupScanWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(TAG.getBytes());

    private final FolderBackupScanNotificationHelper notificationManager;
    private RepoConfig repoConfig;
    private Account account;
    // @FIXME fix this
    private static volatile boolean isWorkerRunning = false;

    private static void setIsRunning(boolean running) {
        isWorkerRunning = running;
    }

    public static boolean isIsWorkerRunning() {
        return isWorkerRunning;
    }

    public FolderBackupScanWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupScanNotificationHelper(context);
    }

    @Override
    public FeatureDataSource getDataSource() {
        return FeatureDataSource.FOLDER_BACKUP;
    }

    private void showNotification() {

        //
        String title = getApplicationContext().getString(R.string.settings_folder_backup_info_title);
        String subTitle = getApplicationContext().getString(R.string.is_scanning);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title, subTitle);
                showForegroundAsync(foregroundInfo);
            } catch (ForegroundServiceStartNotAllowedException e) {
                SLogs.e(e.getMessage());
            }
        } else {
            ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title, subTitle);
            showForegroundAsync(foregroundInfo);
        }
    }

    private static boolean canExc() {
        boolean isRunning = BackupThreadExecutor.getInstance().isFolderBackupRunning();
        return !isRunning;
    }

    @NonNull
    @Override
    public Result doWork() {
        SLogs.d(TAG, "doWork()", "started execution");

        if (!canExc()) {
            SLogs.e(TAG, "The folder scan task was not started, because the folder backup task is running");
            return Result.success();
        }

        setIsRunning(true);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the switch is off");
            return returnSuccess();
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the repo is not selected");
            return returnSuccess();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the folder path is not selected");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.e(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        showNotification();

        //send a scan event
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCANNING);

        // scan all files in the folder
        long lastScanTime = FolderBackupSharePreferenceHelper.readLastScanTime();
        SeafException seafException = FolderScanHelper.traverseBackupPath(backupPaths, account, repoConfig, lastScanTime);
        if (seafException != SeafException.SUCCESS) {
            SafeLogs.e(TAG, "scan failed");
            return returnSuccess();
        }

        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        String content = null;
        if (totalPendingCount > 0) {
            boolean isAllowUpload = checkNetworkTypeIfAllowStartUploadWorker();
            if (!isAllowUpload) {
                content = TransferResult.WAITING.name();
            }
        }

        setIsRunning(false);
        sendCompleteEvent(FeatureDataSource.FOLDER_BACKUP, content, totalPendingCount);
        return Result.success();
    }

    protected Result returnSuccess() {
        setIsRunning(false);
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
        return Result.success();
    }
}
