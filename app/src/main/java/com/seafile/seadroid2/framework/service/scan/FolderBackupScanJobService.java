package com.seafile.seadroid2.framework.service.scan;

import android.app.job.JobParameters;
import android.app.job.JobService;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class FolderBackupScanJobService extends JobService {
    public static final String TAG = "FolderBackupScanJobService";

    @Override
    public boolean onStartJob(JobParameters params) {
        SLogs.d(TAG, "onStartJob()", "started execution");
        CompletableFuture<Integer> future = CompletableFuture.supplyAsync(new Supplier<Integer>() {
            @Override
            public Integer get() {
                return doWork();
            }
        });

        future.whenComplete(new BiConsumer<Integer, Throwable>() {
            @Override
            public void accept(Integer c, Throwable throwable) {
                SafeLogs.d(TAG, "start scan", "backup path file count: ", c + "");
                if (c > 0) {
                    BackgroundJobManagerImpl.getInstance().startFolderBackupChain(true);
                }

                // 标记任务完成
                jobFinished(params, false);
            }
        });
        return true;
    }

    private static boolean canExc() {
        boolean isRunning = BackupThreadExecutor.getInstance().isFolderBackupRunning();
        return !isRunning;
    }

    public int doWork() {
        SafeLogs.e(TAG, "文件夹扫描 Job 启动");
        int backupFileCount = 0;

        if (!canExc()) {
            SLogs.d(TAG, "The folder scan task was not started, because the transfer service is not running");
            return backupFileCount;
        }

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return backupFileCount;
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SLogs.d(TAG, "The folder scan task was not started, because the switch is off");
            return backupFileCount;
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SLogs.d(TAG, "The folder scan task was not started, because the repo is not selected");
            return backupFileCount;
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SLogs.d(TAG, "The folder scan task was not started, because the folder path is not selected");
            return backupFileCount;
        }

        if (!NetworkUtils.isConnected()) {
            SLogs.d(TAG, "network is not connected");
            return backupFileCount;
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return backupFileCount;
            }

            SLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        //scan
        backupFileCount = FolderScanHelper.onlyTraverseBackupPathFileCount(backupPaths, account, repoConfig);
        return backupFileCount;
    }

    @Override
    public boolean onStopJob(JobParameters params) {

        return false;
    }
}
