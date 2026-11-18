package com.seafile.seadroid2.ui.camera_upload;

import android.content.ContentResolver;
import android.text.TextUtils;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

public class CameraUploadManager {
    private final String TAG = "CameraUploadManager";
    public static final String KEY_IS_FULL_SCAN = "com.seafile.seadroid2.key_is_full_scan";
    public static final String AUTHORITY = BuildConfig.APPLICATION_ID + ".cameraupload.provider";
    private static final CameraUploadManager INSTANCE = new CameraUploadManager();

    public static CameraUploadManager getInstance() {
        return INSTANCE;
    }

    public void performSync() {
        performSync(false);
    }

    public void performSync(boolean isFullScan) {
        if (getCameraAccount() == null) {
            SLogs.d(TAG, "Photo backup not started: Account or backup configuration unavailable");
            return;
        }

        if (AlbumBackupSharePreferenceHelper.readBackupAutoPauseSwitch()) {
            SLogs.d(TAG, "Photo backup not started: Automatic upload is paused.");
            return;
        }

        SLogs.d(TAG, "Start the photo backup directly and perform a full scan:" + isFullScan);
        BackupThreadExecutor.getInstance().runAlbumBackupTask(isFullScan);
    }

    /**
     * 设置变更后立即按新配置重启：取消当前任务，旧任务退出后清空待传模型并全量重扫。
     */
    public void restartSync() {
        if (getCameraAccount() == null) {
            SLogs.d(TAG, "Photo backup not restarted: Account or backup configuration unavailable");
            return;
        }

        if (AlbumBackupSharePreferenceHelper.readBackupAutoPauseSwitch()) {
            SLogs.d(TAG, "Photo backup not restarted: Automatic upload is paused.");
            return;
        }

        BackupThreadExecutor.getInstance().restartAlbumBackup();
    }

    /**
     * Get an account that meets the photo backup criteria: Backup enabled, library selected, album selected.
     * */
    public Account getCameraAccount() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null
                || !account.hasValidToken()
                || !AlbumBackupSharePreferenceHelper.readBackupSwitch()) {
            return null;
        }

        RepoConfig config = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (config == null
                || TextUtils.isEmpty(config.getRepoId())
                || !TextUtils.equals(account.getSignature(), config.getSignature())) {
            return null;
        }
        return account;
    }

    public void updateSystemSyncSettings() {
        Account cameraAccount = getCameraAccount();
        try {
            for (Account account : SupportAccountManager.getInstance().getAccountList()) {
                boolean enabled = cameraAccount != null
                        && TextUtils.equals(cameraAccount.getSignature(), account.getSignature());
                android.accounts.Account androidAccount = account.getAndroidAccount();
                int syncable = ContentResolver.getIsSyncable(androidAccount, AUTHORITY);
                if (enabled) {
                    if (syncable <= 0) {
                        ContentResolver.setIsSyncable(androidAccount, AUTHORITY, 1);
                        ContentResolver.setSyncAutomatically(androidAccount, AUTHORITY, true);
                    }
                } else {
                    ContentResolver.cancelSync(androidAccount, AUTHORITY);
                    ContentResolver.setSyncAutomatically(androidAccount, AUTHORITY, false);
                    ContentResolver.setIsSyncable(androidAccount, AUTHORITY, 0);
                }
            }
        } catch (SecurityException e) {
            // The failure to configure the system synchronization entry point cannot prevent direct backup within the application.
            SLogs.d(TAG, "unable to update system synchronization entry：" + e.getMessage());
        }
    }

    public void disableCameraUpload() {
        BackupThreadExecutor.getInstance().stopAlbumBackup();
        updateSystemSyncSettings();
    }
}
