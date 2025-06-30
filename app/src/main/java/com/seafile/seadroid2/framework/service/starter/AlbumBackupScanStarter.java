package com.seafile.seadroid2.framework.service.starter;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.service.scan.AlbumScanHelper;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

public class AlbumBackupScanStarter extends Worker {
    public static final String TAG = "AlbumBackupScanStarter";

    public AlbumBackupScanStarter(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @NonNull
    @Override
    public Result doWork() {
        SafeLogs.d(TAG, "doWork()", "started execution");
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the switch is off");
            return Result.success();
        }

        Account backupAccount = CameraUploadManager.getInstance().getCameraAccount();
        if (backupAccount == null) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the backup account is null");
            return Result.success();
        }

        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || TextUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the repoConfig is null");
            return Result.success();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return Result.success();
        }

        boolean isAllowDataPlan = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return Result.success();
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
        return Result.success();
    }


}
