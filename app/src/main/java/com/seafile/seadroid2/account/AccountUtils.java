package com.seafile.seadroid2.account;

import android.webkit.CookieManager;
import android.webkit.ValueCallback;

import com.blankj.utilcode.util.NotificationUtils;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.GestureLockSharePreferenceHelper;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ssl.CertsManager;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;

public class AccountUtils {

    public static void logout(Account account) {

        // turn off the gesture lock anyway
        GestureLockSharePreferenceHelper.disable();

        Settings.initUserSettings();

        NotificationUtils.cancelAll();

        // sign out operations
        SupportAccountManager.getInstance().signOutAccount(account);
        SupportAccountManager.getInstance().saveCurrentAccount(null);

        // disable camera upload
        CameraUploadManager.getInstance().disableSpecialAccountCameraUpload(account);

        //cancel all jobs
        BackgroundJobManagerImpl.getInstance().cancelAllJobs();

        //reset IO instance for new account
        HttpIO.resetLoggedInInstance();

        //clear instance
        DataStoreManager.resetUserInstance();

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

        NotificationUtils.cancelAll();

        //
        Settings.initUserSettings();

        //switch camera upload
        CameraUploadManager.getInstance().setCameraAccount(account);

        //cancel all jobs
        BackgroundJobManagerImpl.getInstance().cancelAllJobs();

        //reset IO instance for new account
        HttpIO.resetLoggedInInstance();

        //clear instance
        DataStoreManager.resetUserInstance();

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
