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
    // @FIXME fix this
    private static volatile boolean isWorkerRunning = false;

    private static void setIsRunning(boolean running) {
        isWorkerRunning = running;
    }

    public static boolean isIsWorkerRunning() {
        return isWorkerRunning;
    }
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
            return returnSuccess();
        }

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


        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        //scan
        int count = FolderScanHelper.onlyTraverseBackupPathFileCount(backupPaths, account, repoConfig);
        if (count == 0) {
            SLogs.d(TAG, "The folder scan task was not started, because no new files were found");
            return returnSuccess();
        }

        SLogs.d(TAG, "start scan", "backup path file count: ", GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getTotalCount() + "");
        BackgroundJobManagerImpl.getInstance().startFolderBackupChain(true);

        return returnSuccess();
    }

    protected Result returnSuccess() {
        setIsRunning(false);
        return Result.success();
    }
}
