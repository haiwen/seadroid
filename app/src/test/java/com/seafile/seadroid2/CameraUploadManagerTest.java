package com.seafile.seadroid2;

import android.content.ContentResolver;
import android.content.SyncResult;
import android.os.Bundle;

import com.elvishew.xlog.LogLevel;
import com.elvishew.xlog.XLog;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.camera_upload.AlbumBackupAdapter;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;
import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;
import org.robolectric.util.ReflectionHelpers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 28, application = CameraUploadManagerTest.TestApplication.class,
        shadows = CameraUploadManagerTest.ShadowBackupThreadExecutor.class)
public class CameraUploadManagerTest {
    private Account account;
    private SupportAccountManager accountManager;
    private CameraUploadManager manager;

    public static class TestApplication extends SeadroidApplication {
        @Override
        public void onCreate() {
            // 保留真实的应用上下文，但不启动生产环境服务。
        }
    }

    @Implements(value = BackupThreadExecutor.class, isInAndroidSdk = false)
    public static class ShadowBackupThreadExecutor {
        static int albumBackupStarts;
        static boolean fullScan;

        @Implementation
        protected void runAlbumBackupTask(boolean full) {
            albumBackupStarts++;
            fullScan = full;
        }
    }

    @Before
    public void setUp() {
        XLog.init(LogLevel.NONE);
        ReflectionHelpers.setStaticField(SupportAccountManager.class, "singleton", null);
        ReflectionHelpers.setStaticField(BackupThreadExecutor.class, "singleton", null);
        ReflectionHelpers.setStaticField(Settings.class, "_account", null);
        DataStoreManager.resetUserInstance();
        ShadowBackupThreadExecutor.albumBackupStarts = 0;
        ShadowBackupThreadExecutor.fullScan = false;
        account = new Account("https://backup.example.com/", "camera@example.com",
                "Camera", null, "test-token", false);
        accountManager = SupportAccountManager.getInstance();
        registerAccount(account);
        accountManager.saveCurrentAccount(account.getSignature());
        Settings.initUserSettings();
        AlbumBackupSharePreferenceHelper.writeBackupSwitch(true);
        Settings.ALBUM_BACKUP_PAUSE_AUTO_UPLOAD_SWITCH.putValue(false);
        AlbumBackupSharePreferenceHelper.writeRepoConfig(repoConfig("camera-repo", account));
        manager = CameraUploadManager.getInstance();
        assertNotNull(accountManager.getCurrentAccount());
    }

    @Test
    public void matchingAccountAndConfigCanStartBackup() {
        assertEquals(account, manager.getCameraAccount());

        manager.performSync();
        assertEquals(1, ShadowBackupThreadExecutor.albumBackupStarts);
        assertFalse(ShadowBackupThreadExecutor.fullScan);

        manager.performSync(true);
        assertEquals(2, ShadowBackupThreadExecutor.albumBackupStarts);
        assertTrue(ShadowBackupThreadExecutor.fullScan);
    }

    @Test
    public void systemSyncDisabledDoesNotHideCameraAccount() {
        ContentResolver.setMasterSyncAutomatically(false);
        assertFalse(ContentResolver.getMasterSyncAutomatically());

        assertEquals(account, manager.getCameraAccount());
    }

    @Test
    public void disabledBackupSwitchRejectsAccountAndDoesNotStartBackup() {
        AlbumBackupSharePreferenceHelper.writeBackupSwitch(false);

        assertNull(manager.getCameraAccount());
        manager.performSync(true);
        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
    }

    @Test
    public void accountWithoutTokenCannotBackUp() {
        accountManager.updateAuthToken(account.getAndroidAccount(), Authenticator.AUTHTOKEN_TYPE, null);
        assertFalse(accountManager.getSeafileAccount(account.getAndroidAccount()).hasValidToken());

        assertNull(manager.getCameraAccount());
        manager.performSync();
        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
    }

    @Test
    public void repoConfigFromAnotherAccountCannotBackUp() {
        Account other = new Account(account.getServer(), "other@example.com",
                "Other", null, "other-test-token", false);
        registerAccount(other);
        AlbumBackupSharePreferenceHelper.writeRepoConfig(repoConfig("other-repo", other));

        assertEquals(account, accountManager.getCurrentAccount());
        assertNull(manager.getCameraAccount());
        manager.performSync();
        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
    }

    @Test
    public void missingRepoConfigCannotBackUp() {
        AlbumBackupSharePreferenceHelper.writeRepoConfig(null);

        assertNull(manager.getCameraAccount());
        manager.performSync();
        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
    }

    @Test
    public void emptyRepoIdCannotBackUp() {
        for (String repoId : new String[]{null, ""}) {
            AlbumBackupSharePreferenceHelper.writeRepoConfig(repoConfig(repoId, account));

            assertNull(manager.getCameraAccount());
            manager.performSync(true);
            assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
        }
    }

    @Test
    public void pausedBackupKeepsAccountButDoesNotStartExecution() {
        Settings.ALBUM_BACKUP_PAUSE_AUTO_UPLOAD_SWITCH.putValue(true);
        assertEquals(account, manager.getCameraAccount());

        manager.performSync();
        manager.performSync(true);

        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
    }

    @Test
    public void systemCallbackForCurrentAccountPassesFullScanToExecutor() {
        AlbumBackupAdapter adapter = new AlbumBackupAdapter(SeadroidApplication.getAppContext());
        Bundle extras = new Bundle();
        extras.putBoolean(CameraUploadManager.KEY_IS_FULL_SCAN, true);
        SyncResult syncResult = new SyncResult();

        adapter.onPerformSync(account.getAndroidAccount(), extras,
                CameraUploadManager.AUTHORITY, null, syncResult);

        assertEquals(1, ShadowBackupThreadExecutor.albumBackupStarts);
        assertTrue(ShadowBackupThreadExecutor.fullScan);
        assertEquals(0, syncResult.stats.numAuthExceptions);

        adapter.onPerformSync(account.getAndroidAccount(), new Bundle(),
                CameraUploadManager.AUTHORITY, null, syncResult);

        assertEquals(2, ShadowBackupThreadExecutor.albumBackupStarts);
        assertFalse(ShadowBackupThreadExecutor.fullScan);
    }

    @Test
    public void systemCallbackForOtherLoggedInAccountDoesNotStartBackup() {
        Account other = new Account(account.getServer(), "other@example.com",
                "Other", null, "other-test-token", false);
        registerAccount(other);
        assertTrue(accountManager.getSeafileAccount(other.getAndroidAccount()).hasValidToken());
        SyncResult syncResult = new SyncResult();
        AlbumBackupAdapter adapter = new AlbumBackupAdapter(SeadroidApplication.getAppContext());

        adapter.onPerformSync(other.getAndroidAccount(), new Bundle(),
                CameraUploadManager.AUTHORITY, null, syncResult);

        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
        assertEquals(0, syncResult.stats.numAuthExceptions);
    }

    @Test
    public void systemCallbackWithoutTokenRecordsAuthFailureAndDoesNotStartBackup() {
        accountManager.updateAuthToken(account.getAndroidAccount(), Authenticator.AUTHTOKEN_TYPE, null);
        assertFalse(accountManager.getSeafileAccount(account.getAndroidAccount()).hasValidToken());
        SyncResult syncResult = new SyncResult();
        AlbumBackupAdapter adapter = new AlbumBackupAdapter(SeadroidApplication.getAppContext());

        adapter.onPerformSync(account.getAndroidAccount(), new Bundle(),
                CameraUploadManager.AUTHORITY, null, syncResult);

        assertEquals(1, syncResult.stats.numAuthExceptions);
        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
    }

    @Test
    public void pausedSystemCallbackDoesNotStartBackup() {
        Settings.ALBUM_BACKUP_PAUSE_AUTO_UPLOAD_SWITCH.putValue(true);
        Bundle extras = new Bundle();
        extras.putBoolean(CameraUploadManager.KEY_IS_FULL_SCAN, true);
        SyncResult syncResult = new SyncResult();
        AlbumBackupAdapter adapter = new AlbumBackupAdapter(SeadroidApplication.getAppContext());

        adapter.onPerformSync(account.getAndroidAccount(), extras,
                CameraUploadManager.AUTHORITY, null, syncResult);

        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
        assertEquals(0, syncResult.stats.numAuthExceptions);
    }

    @Test
    public void appSyncRunsWithoutChangingDisabledSystemSyncSettings() {
        ContentResolver.setIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, 0);
        ContentResolver.setSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, false);
        ContentResolver.setMasterSyncAutomatically(false);

        manager.performSync(true);

        assertEquals(1, ShadowBackupThreadExecutor.albumBackupStarts);
        assertTrue(ShadowBackupThreadExecutor.fullScan);
        assertEquals(0, ContentResolver.getIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getMasterSyncAutomatically());
    }

    @Test
    public void updateRegistersOnlyCurrentAccountAndPreservesUserSyncSwitches() {
        Account other = new Account(account.getServer(), "other@example.com",
                "Other", null, "other-test-token", false);
        registerAccount(other);
        ContentResolver.setIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, 0);
        ContentResolver.setSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, false);
        ContentResolver.setIsSyncable(other.getAndroidAccount(), CameraUploadManager.AUTHORITY, 1);
        ContentResolver.setSyncAutomatically(other.getAndroidAccount(), CameraUploadManager.AUTHORITY, true);
        ContentResolver.setMasterSyncAutomatically(false);

        manager.updateSystemSyncSettings();

        assertEquals(1, ContentResolver.getIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertTrue(ContentResolver.getSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertEquals(0, ContentResolver.getIsSyncable(other.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getSyncAutomatically(other.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getMasterSyncAutomatically());

        // 已注册账户再次更新时，不覆盖用户主动关闭的自动同步开关。
        ContentResolver.setSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, false);
        manager.updateSystemSyncSettings();

        assertEquals(1, ContentResolver.getIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertEquals(0, ContentResolver.getIsSyncable(other.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getSyncAutomatically(other.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getMasterSyncAutomatically());
        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
    }

    @Test
    public void disablingBusinessBackupAndUpdatingDisablesSystemEntry() {
        manager.updateSystemSyncSettings();
        assertEquals(1, ContentResolver.getIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertTrue(ContentResolver.getSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        AlbumBackupSharePreferenceHelper.writeBackupSwitch(false);

        manager.updateSystemSyncSettings();

        assertEquals(0, ContentResolver.getIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        assertFalse(ContentResolver.getSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY));
        SyncResult syncResult = new SyncResult();
        AlbumBackupAdapter adapter = new AlbumBackupAdapter(SeadroidApplication.getAppContext());
        adapter.onPerformSync(account.getAndroidAccount(), new Bundle(),
                CameraUploadManager.AUTHORITY, null, syncResult);
        assertEquals(0, ShadowBackupThreadExecutor.albumBackupStarts);
        assertEquals(0, syncResult.stats.numAuthExceptions);
    }

    private void registerAccount(Account value) {
        Bundle userData = new Bundle();
        userData.putString(Authenticator.KEY_SERVER_URI, value.getServer());
        userData.putString(Authenticator.KEY_EMAIL, value.getEmail());
        userData.putString(Authenticator.KEY_CONTACT_EMAIL, value.getContactEmail());
        userData.putString(Authenticator.KEY_NAME, value.getName());
        assertTrue(accountManager.addAccountExplicitly(value.getAndroidAccount(), null, userData));
        accountManager.updateAuthToken(value.getAndroidAccount(), Authenticator.AUTHTOKEN_TYPE, value.getToken());
    }

    private RepoConfig repoConfig(String repoId, Account owner) {
        return new RepoConfig(repoId, "Camera Uploads", owner.getEmail(), owner.getSignature());
    }
}
