package com.seafile.seadroid2.framework.service.scan;

import android.app.job.JobParameters;
import android.app.job.JobService;

import androidx.work.ListenableWorker;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

@Deprecated
public class FolderBackupScanJobService extends JobService {
    public static final String TAG = "JobSchedulerHelper-FolderBackupScanJobService";

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
                Toasts.show("扫描到的文件夹数量：" + c);
                SafeLogs.d(TAG, "start scan", "backup path file count: ", c + "");
                if (c > 0) {
                    TransferService.restartFolderBackupService(getApplicationContext(), false);
                }

                // 标记任务完成
                jobFinished(params, false);
            }
        });
        return true;
    }

    public int doWork() {
        int backupPathFileCount = 0;
        Toasts.show("文件夹扫描");

        SLogs.d(TAG, "doWork()", "started execution");
        boolean isServiceRunning = TransferService.getServiceRunning();
        if (isServiceRunning) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the transfer service is running");
            return backupPathFileCount;
        }

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return backupPathFileCount;
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the switch is off");
            return backupPathFileCount;
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the repo is not selected");
            return backupPathFileCount;
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the folder path is not selected");
            return backupPathFileCount;
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return backupPathFileCount;
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return backupPathFileCount;
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }


        //scan
        backupPathFileCount = FolderScanHelper.traverseBackupPathFileCount(backupPaths, account, repoConfig);
        return backupPathFileCount;
    }

    @Override
    public boolean onStopJob(JobParameters params) {
        return false;
    }
}
