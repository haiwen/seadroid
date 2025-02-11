package com.seafile.seadroid2.ui.camera_upload;

import android.content.ContentResolver;
import android.os.Bundle;
import android.util.Log;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;

import java.util.List;


/**
 * Camera Upload Manager.
 * <p/>
 * This class can be used by other parts of Seadroid to enable/configure the camera upload
 * service.
 */
public class CameraUploadManager {

    private static final CameraUploadManager INSTANCE = new CameraUploadManager();

    public static CameraUploadManager getInstance() {
        return INSTANCE;
    }

    /**
     * The authority of the camera sync service
     */
    public static final String AUTHORITY = BuildConfig.APPLICATION_ID + ".cameraupload.provider";

    /**
     * Initiate a camera sync immediately.
     */
    public void performSync() {
        Account cameraAccount = getCameraAccount();
        if (cameraAccount == null) {
            SLogs.d("No one has turned on album backup");
            return;
        }

        ContentResolver.requestSync(cameraAccount.getAndroidAccount(), AUTHORITY, Bundle.EMPTY);
    }

    public void performSync(boolean isForce) {

        Log.d(CameraUploadManager.class.getName(), "performSyncByStatus()");

        Bundle b = new Bundle();
        b.putBoolean(ContentResolver.SYNC_EXTRAS_MANUAL, isForce);
        Account cameraAccount = getCameraAccount();
        if (cameraAccount != null) {
            ContentResolver.requestSync(cameraAccount.getAndroidAccount(), AUTHORITY, b);
        }
    }

    /**
     * Is camera upload enabled?
     *
     * @return true if camera upload is enabled.
     */
    public boolean isCameraUploadEnabled() {
        Account account = getCameraAccount();
        return account != null;
    }

    /**
     * Get the account that is currently the remote target for the camera upload
     *
     * @return the account if camera is enabled, null otherwise.
     */
    public Account getCameraAccount() {
        List<Account> list = SupportAccountManager.getInstance().getAccountList();
        for (Account account : list) {
            int isSyncable = ContentResolver.getIsSyncable(account.getAndroidAccount(), AUTHORITY);
            if (isSyncable > 0)
                return account;
        }
        return null;
    }

    /**
     * Change the account currently responsible for camera upload.
     *
     * @param account An account. must not be null.
     */
    public void setCameraAccount(Account account) {
        List<Account> list = SupportAccountManager.getInstance().getAccountList();
        for (Account a : list) {
            if (a.equals(account)) {
                // enable camera upload on this account
                ContentResolver.setIsSyncable(a.getAndroidAccount(), AUTHORITY, 1);
                ContentResolver.setSyncAutomatically(a.getAndroidAccount(), AUTHORITY, true);
            } else {
                // disable on all the others
                ContentResolver.cancelSync(a.getAndroidAccount(), AUTHORITY);
                ContentResolver.setIsSyncable(a.getAndroidAccount(), AUTHORITY, 0);
            }
        }
    }

    /**
     * Disable camera upload.
     */
    public void disableCameraUpload() {
        List<Account> list = SupportAccountManager.getInstance().getAccountList();
        for (Account account : list) {
            ContentResolver.cancelSync(account.getAndroidAccount(), AUTHORITY);
            ContentResolver.setIsSyncable(account.getAndroidAccount(), AUTHORITY, 0);
        }

        BackgroundJobManagerImpl.getInstance().cancelMediaBackupWorker();
    }

    /**
     * Disable camera upload.
     */
    public void disableSpecialAccountCameraUpload(Account forAccount) {
        List<Account> list = SupportAccountManager.getInstance().getAccountList();
        for (Account account : list) {
            if (forAccount.equals(account)) {
                ContentResolver.cancelSync(account.getAndroidAccount(), AUTHORITY);
                ContentResolver.setIsSyncable(account.getAndroidAccount(), AUTHORITY, 0);
            }
        }
    }
}
