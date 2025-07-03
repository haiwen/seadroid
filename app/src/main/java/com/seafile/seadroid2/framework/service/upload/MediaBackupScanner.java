package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.text.TextUtils;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.ParentEventTransfer;
import com.seafile.seadroid2.framework.service.scan.AlbumScanHelper;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

public class MediaBackupScanner extends ParentEventTransfer {
    private final String TAG = "MediaBackupScanner";

    public MediaBackupScanner(Context context) {
        super(context);
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
        return SeafException.SUCCESS;
    }

    public SeafException scan(boolean isForce) {
        SafeLogs.e(TAG, "相册扫描器启动");

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
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        if (isForce) {
            AlbumBackupSharePreferenceHelper.resetLastScanTime();
        }

        SafeLogs.d(TAG, "doWork()", "start scan");
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCANNING);

        SeafException seafException = AlbumScanHelper.loadMedia(getContext(), account, repoConfig);
        if (seafException != SeafException.SUCCESS) {
            SafeLogs.d(TAG, "doWork()", "loadMedia() failed：" + seafException.getMessage());
            send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
            return seafException;
        }

        //
        int totalPendingCount = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getPendingCount();
        String content = null;
        if (totalPendingCount > 0) {
            boolean isAllowUpload = AlbumScanHelper.checkNetworkTypeIfAllowStartUploadWorker();
            if (!isAllowUpload) {
                content = TransferResult.WAITING.name();
            }
        }

        //
        sendCompleteEvent(FeatureDataSource.ALBUM_BACKUP, content, totalPendingCount);

        return SeafException.SUCCESS;
    }

}
