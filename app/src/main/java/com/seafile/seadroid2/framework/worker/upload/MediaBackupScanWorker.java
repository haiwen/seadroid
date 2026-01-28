package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.AlbumBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.service.scan.AlbumScanHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.UUID;

public class MediaBackupScanWorker extends BaseScanWorker {
    public static final String TAG = "MediaBackupScanWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(TAG.getBytes());

    private final AlbumBackupScanNotificationHelper notificationManager;

    public static final String BASE_DIR = "My Photos";

    // @FIXME fix this
    private static volatile boolean isWorkerRunning = false;

    private static void setIsRunning(boolean running) {
        isWorkerRunning = running;
    }
    public static boolean isIsWorkerRunning(){
        return isWorkerRunning;
    }

    public MediaBackupScanWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new AlbumBackupScanNotificationHelper(context);
    }


    private void showNotification() {

        String title = getApplicationContext().getString(R.string.settings_camera_upload_info_title);
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

    @Override
    public FeatureDataSource getDataSource() {
        return FeatureDataSource.ALBUM_BACKUP;
    }


    private static boolean canExc() {
        boolean isRunning = BackupThreadExecutor.getInstance().isAlbumBackupRunning();
        return !isRunning;
    }

    @NonNull
    @Override
    public Result doWork() {
        SLogs.d(TAG, "doWork()", "started execution");

        if (!canExc()) {
            SafeLogs.e(TAG, "doWork()", "The album scan task was not started, because the album backup thread is running");
            return Result.success();
        }

        setIsRunning(true);

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

        // force scan all files
        long lastScanTime = AlbumBackupSharePreferenceHelper.readLastScanTimeMills();

        SeafException seafException = AlbumScanHelper.loadMedia(getApplicationContext(), account, repoConfig, lastScanTime);
        if (seafException != SeafException.SUCCESS) {
            SafeLogs.d(TAG, "doWork()", "loadMedia() failed：" + seafException.getMessage());
            send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
            return returnSuccess();
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
        setIsRunning(false);
        sendCompleteEvent(FeatureDataSource.ALBUM_BACKUP, content, totalPendingCount);
        return Result.success();
    }

    protected Result returnSuccess() {
        setIsRunning(false);
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
        return Result.success();
    }
}
