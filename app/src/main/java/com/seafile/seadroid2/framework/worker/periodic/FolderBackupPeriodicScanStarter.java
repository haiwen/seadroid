package com.seafile.seadroid2.framework.worker.periodic;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.service.scan.FolderScanHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.UUID;

public class FolderBackupPeriodicScanStarter extends Worker {
    public static final String TAG = "FolderBackupScanStarter";
    public static final UUID UID = UUID.nameUUIDFromBytes(FolderBackupPeriodicScanStarter.class.getSimpleName().getBytes());

    public FolderBackupPeriodicScanStarter(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    private static boolean canExc() {
        boolean isRunning = BackupThreadExecutor.getInstance().isFolderBackupRunning();
        return !isRunning;
    }

    @NonNull
    @Override
    public Result doWork() {
        SLogs.e(TAG, "文件夹扫描 Worker 启动");

        if (!canExc()) {
            SLogs.e(TAG, "The folder scan task was not started, because the folder backup task is running");
            return Result.success();
        }

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SLogs.d(TAG, "The folder scan task was not started, because the switch is off");
            return Result.success();
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SLogs.d(TAG, "The folder scan task was not started, because the repo is not selected");
            return Result.success();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SLogs.d(TAG, "The folder scan task was not started, because the folder path is not selected");
            return Result.success();
        }

        if (!NetworkUtils.isConnected()) {
            SLogs.d(TAG, "network is not connected");
            return Result.success();
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SLogs.e(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return Result.success();
            }

            SLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }


        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        //scan
        int count = FolderScanHelper.traverseBackupPathFileCount(backupPaths, account, repoConfig);
        if (count == 0) {
            SLogs.d(TAG, "The folder scan task was not started, because no new files were found");
            return Result.success();
        }

        SLogs.d(TAG, "start scan", "backup path file count: ", GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getTotalCount() + "");
        BackgroundJobManagerImpl.getInstance().startFolderBackupChain(true);

        return Result.success();
    }
}
