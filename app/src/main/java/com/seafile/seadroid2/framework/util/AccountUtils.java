package com.seafile.seadroid2.framework.util;

import android.webkit.CookieManager;
import android.webkit.ValueCallback;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.GestureLockManager;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;

public class AccountUtils {

    public static void logout(Account account) {

        // turn off the gesture lock anyway
        GestureLockManager.writeGestureLockSwitch(false);

        // sign out operations
        SupportAccountManager.getInstance().signOutAccount(account);

        // disable camera upload
        CameraUploadManager.getInstance().disableSpecialAccountCameraUpload(account);

        //cancel all jobs
        BackgroundJobManagerImpl.getInstance().cancelAllJobs();

        //reset IO instance for new account
        IO.resetLoggedInInstance();

        //clear instance
        DataStoreManager.resetUserInstance();
        FolderBackupManager.resetUserInstance();
        AlbumBackupManager.resetUserInstance();


        //clear cookie
        CookieManager cookieManager = CookieManager.getInstance();
        cookieManager.removeAllCookies(new ValueCallback<Boolean>() {
            @Override
            public void onReceiveValue(Boolean value) {
                SLogs.d("removeAllCookie? " + value);
            }
        });
    }

    public static void switchAccount(Account account) {
        if (account == null) {
            return;
        }

        // update current Account info
        SupportAccountManager.getInstance().saveCurrentAccount(account.getSignature());

        //switch camera upload
        CameraUploadManager.getInstance().setCameraAccount(account);

        //cancel all jobs
        BackgroundJobManagerImpl.getInstance().cancelAllJobs();

        //reset IO instance for new account
        IO.resetLoggedInInstance();

        //clear instance
        DataStoreManager.resetUserInstance();
        FolderBackupManager.resetUserInstance();
        AlbumBackupManager.resetUserInstance();

        //clear cookie
        CookieManager cookieManager = CookieManager.getInstance();
        cookieManager.removeAllCookies(new ValueCallback<Boolean>() {
            @Override
            public void onReceiveValue(Boolean value) {
                SLogs.d("removeAllCookie? " + value);
            }
        });
    }
}
