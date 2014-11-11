package com.seafile.seadroid2;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;

/**
 * Access the app settings
 */
public final class SettingsManager {
    private static final String DEBGUG_TAG = "SettingsManager";

    // Global variables
    private SharedPreferences sharedPref = SeadroidApplication.getAppContext()
            .getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);

    private SharedPreferences.Editor editor = sharedPref.edit();
    private static SettingsManager instance;

    private SettingsManager() {
    }

    private SharedPreferences settingsSharedPref = PreferenceManager
            .getDefaultSharedPreferences(SeadroidApplication.getAppContext());

    // Gesture Lock
    public static final String GESTURE_LOCK_SWITCH_KEY = "gesture_lock_switch_key";
    public static final String GESTURE_LOCK_KEY = "gesture_lock_key";
    public static final int GESTURE_LOCK_REQUEST = 1;

    // Camera upload
    public static final String PKG = "com.seafile.seadroid2";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_ID = PKG + ".camera.repoid";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_NAME = PKG + ".camera.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL = PKG + ".camera.account.email";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER = PKG + ".camera.account.server";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN = PKG + ".camera.account.token";
    public static final String ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY = "allow_mobile_connections_switch_key";
    public static final String CAMERA_UPLOAD_SWITCH_KEY = "camera_upload_switch_key";
    public static final String CAMERA_UPLOAD_REPO_KEY = "camera_upload_repo_key";
    public static final int CHOOSE_CAMERA_UPLOAD_REPO_REQUEST = 2;

    // About tab
    public static final String SETTINGS_ABOUT_VERSION_KEY = "settings_about_version_key";

    public static long lock_timestamp = 0;
    public static final long LOCK_EXPIRATION_MSECS = 5 * 60 * 1000;

    public static synchronized SettingsManager instance() {
        if (instance == null) {
            instance = new SettingsManager();
        }

        return instance;
    }

    public void setupGestureLock() {
        settingsSharedPref.edit().putBoolean(GESTURE_LOCK_SWITCH_KEY, true)
                .commit();
        saveGestureLockTimeStamp();
    }
    
    public boolean isGestureLockEnabled() {
        return settingsSharedPref.getBoolean(GESTURE_LOCK_SWITCH_KEY, false);
    }

    public boolean isGestureLockRequired() {
        if (!isGestureLockEnabled()) {
            return false;
        }
        LockPatternUtils mLockPatternUtils = new LockPatternUtils(SeadroidApplication.getAppContext());
        if (!mLockPatternUtils.savedPatternExists()) {
            return false;
        }
        long now = System.currentTimeMillis();
        if (now < lock_timestamp + LOCK_EXPIRATION_MSECS) {
            return false;
        }

        return true;
    }

    public void saveGestureLockTimeStamp() {
        lock_timestamp = System.currentTimeMillis();
    }

    public void saveCameraUploadRepoName(String repoName) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, repoName);
        editor.commit();
    }

    public String getCameraUploadRepoName() {
        return sharedPref.getString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);
    }

    public void saveCameraUploadRepoInfo(String repoId, String repoName,
            String dstDir, Account account) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, repoId);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, repoName);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, account.getEmail());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, account.getServer());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, account.getToken());
        editor.commit();
    }

    void clearCameraUploadInfo() {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
        editor.commit();
    }

    public boolean checkNetworkStatus() {
        if (!Utils.isNetworkOn()) {
            return false;
        }
        // user does not allow mobile connections
        if (!Utils.isWiFiOn() && !isAllowMobileConnections()) {
            return false;
        }
        // Wi-Fi or 2G/3G/4G connections available
        return true;
    }
    
    
    public boolean isUploadStart() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_SWITCH_KEY, false);
    }

    public boolean isAllowMobileConnections() {
        return settingsSharedPref.getBoolean(ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY, false);
    }

    public String getCameraUploadRepoId() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
    }

    public String getCameraUploadAccountEmail() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);
    }

    public String getCameraUploadAccountServer() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
    }

    public String getCameraUploadAccountToken() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, null);
    }
}
