package com.seafile.seadroid2;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

import com.seafile.seadroid2.R.string;
import com.seafile.seadroid2.account.Account;

/**
 * Access the app settings
 */
public final class SettingsManager {
    private static final String DEBGUG_TAG = "SettingsManager";
    
    // Global variables
    private SharedPreferences sharedPref = SeadroidApplication
            .getAppContext()
            .getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
    
    private SharedPreferences.Editor editor = sharedPref.edit();
    private static SettingsManager instance;
    private SettingsManager() {}
    
    private SharedPreferences settingsSharedPref = PreferenceManager
            .getDefaultSharedPreferences(SeadroidApplication.getAppContext());

    // Gesture Lock
    public static final String GESTURE_LOCK_SWITCH_KEY = "gesture_lock_switch_key";
    public static final String GESTURE_LOCK_TIMESTAMP = "getsture_lock_timestamp";
    public static final String GESTURE_LOCK_KEY = "gesture_lock_key";
    public static final int GESTURE_LOCK_REQUEST = 1;
    
    // Camera upload
    public static final String PKG = "com.seafile.seadroid2";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_ID = PKG + ".camera.repoid";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_NAME = PKG + ".camera.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL = PKG + ".camera.account.email";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER = PKG + ".camera.account.server";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN = PKG + ".camera.account.token";
    public static final String SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME = PKG + ".camera.settings.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_SETTINGS_START = PKG + ".camera.settings.startService";
    public static final String ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY = "allow_mobile_connections_switch_key";
    public static final String CAMERA_UPLOAD_SWITCH_KEY = "camera_upload_switch_key";
    public static final String CAMERA_UPLOAD_REPO_KEY = "camera_upload_repo_key";
    public static final int CHOOSE_CAMERA_UPLOAD_REPO_REQUEST = 2;
    
    // About tab
    public static final String SETTINGS_ABOUT_VERSION_KEY = "settings_about_version_key";
    
    
    public static synchronized SettingsManager instance() {
        if (instance == null) {
            instance = new SettingsManager();
        }

        return instance;
    }

    public boolean isGestureLockEnabled() {
        
        return settingsSharedPref.getBoolean(GESTURE_LOCK_SWITCH_KEY, false);
    }
    
    public void setGestureLockPattern(String pattern) {
        editor.putString(GESTURE_LOCK_KEY, pattern);
        editor.commit();
        saveGestureLockTimeStamp();
    }

    public String getGestureLockPattern() {
        return sharedPref.getString(GESTURE_LOCK_KEY, null);
    }
    
    public boolean isGestureLockLocked() {
        if (!isGestureLockEnabled()) {
            return false;
        }
        if (getGestureLockPattern() == null
                || getGestureLockPattern().equals("")) {
            return false;
        }
        
        // IMPORTANT NOTE
        // please use seconds to calculate expiration time period, like 5 mins = 60 * 5
        // expiration time is less than or equals 5 mins here
        if ((System.currentTimeMillis()/1000 - sharedPref.getLong(GESTURE_LOCK_TIMESTAMP, 0)) <= 60 * 5 ) {
            return false;
        }

        return true;
    }
    
    public void saveGestureLockTimeStamp() {
        Long tsLong = System.currentTimeMillis()/1000;
        editor.putLong(GESTURE_LOCK_TIMESTAMP, tsLong);
        editor.commit();
    }
	
    public void saveCameraUploadRepoName(String repoName) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, repoName);
        editor.commit();
    }

    public String getCameraUploadRepoName() {
        return sharedPref.getString(
                SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, null);
    }
    
    public void saveCameraUploadRepoInfo(String repoId, String repoName,
            String dstDir, Account account) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, repoId);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, repoName);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL,
                account.getEmail());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER,
                account.getServer());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN,
                account.getToken());
        editor.commit();
    }

    public boolean isUploadStart() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_SWITCH_KEY, false);
    }
    
    public boolean isAllowMobileConnections() {
        return settingsSharedPref.getBoolean(ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY, false);
    }
    
    public String getRepoId() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
    }
    
    public String getRepoName() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);
    }
    
    public String getAccountEmail () {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);
        
    }
    
    public String getAccountServer() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
    }
    
    public String getAccountToken() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, null);
    }

}
