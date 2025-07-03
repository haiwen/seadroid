package com.seafile.seadroid2.framework.service.scan;

import android.app.job.JobParameters;
import android.app.job.JobService;
import android.text.TextUtils;

import androidx.work.ListenableWorker;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class AlbumBackupScanJobService extends JobService {
    public static final String TAG = "AlbumBackupScanJobService";

    @Override
    public boolean onStartJob(JobParameters params) {
        SLogs.d(TAG, "onStartJob()", "started execution");
        CompletableFuture<Boolean> future = CompletableFuture.supplyAsync(new Supplier<Boolean>() {
            @Override
            public Boolean get() {
                return doWork();
            }
        });

        future.whenComplete(new BiConsumer<Boolean, Throwable>() {
            @Override
            public void accept(Boolean c, Throwable throwable) {
                SafeLogs.e(TAG, "start scan", "backup state", c + "");
                if (c) {
                    TransferService.restartPhotoBackupService(getApplicationContext());
                }

                // 标记任务完成
                jobFinished(params, false);
            }
        });
        return true;
    }

    @Override
    public boolean onStopJob(JobParameters params) {
        return false;
    }

    private static boolean canExc() {
        if (!TransferService.getServiceRunning()) {
            return true;
        }

        CompletableFuture<Void> future = TransferService.getActiveTasks().getOrDefault(FeatureDataSource.ALBUM_BACKUP, null);
        return future == null || future.isDone();
    }

    private boolean doWork() {
        SafeLogs.e(TAG, "相册扫描 Job 启动");

        if (!canExc()) {
            SafeLogs.d(TAG, "doWork()", "The album scan task was not started, because the transfer service is not running");
            return false;
        }

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return false;
        }

        boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the switch is off");
            return false;
        }

        Account backupAccount = CameraUploadManager.getInstance().getCameraAccount();
        if (backupAccount == null) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the backup account is null");
            return false;
        }

        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || TextUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the repoConfig is null");
            return false;
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return false;
        }

        boolean isAllowDataPlan = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return false;
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        SafeLogs.d(TAG, "doWork()", "start scan");

        boolean result = AlbumScanHelper.readMediaResult(getApplicationContext(), account, repoConfig);

        if (result) {
            TransferService.restartPhotoBackupService(getApplicationContext());
            SafeLogs.d(TAG, "doWork()", "scan success");
        } else {
            SafeLogs.d(TAG, "doWork()", "scan failed");
        }

        return result;
    }
}
