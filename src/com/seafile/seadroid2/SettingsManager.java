package com.seafile.seadroid2;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

import com.seafile.seadroid2.account.Account;

/**
 * Access the app settings
 */
public final class SettingsManager {

    // Gesture Lock
	public static final String GESTURE_LOCK_SWITCH_KEY = "gesture_lock_switch_key";
	public static final String LOCK_KEY = "gesture_lock_key";
	public static final int GESTURE_LOCK_REQUEST = 1;

    // Camera upload
	public static final String PKG = "com.seafile.seadroid2";
    public static final String EXTRA_CAMERA_UPLOAD = PKG + ".camera.upload";
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
    
    private SharedPreferences sharedPref = SeadroidApplication
        .getAppContext()
        .getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);

    private SharedPreferences.Editor editor = sharedPref.edit();
    private static SettingsManager instance;

    private SharedPreferences settings = PreferenceManager
    		.getDefaultSharedPreferences(SeadroidApplication.getAppContext());
    private SharedPreferences.Editor settingsEditor = settings.edit();
    
    public static synchronized SettingsManager instance() {
        if (instance == null) {
            instance = new SettingsManager();
        }

        return instance;
    }

    public boolean isGestureLockEnabled() {
    	
        return settings.getBoolean(SettingsManager.GESTURE_LOCK_SWITCH_KEY, false);
    }
    
    public void saveCameraUploadRepoName(String repoName) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, repoName);
        editor.commit();
    }

    public String getCameraUploadRepoName() {
        return sharedPref.getString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, null);
    }
    
    public void saveCameraUploadRepoInfo(String repoId, String repoName, String dstDir,
            Account account) {
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
		return settings.getBoolean(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY, false);
	}

	public void clearGestureLock() {
		settingsEditor.putBoolean(SettingsManager.GESTURE_LOCK_SWITCH_KEY, false);
		settingsEditor.putString(SettingsManager.LOCK_KEY, null);
		settingsEditor.commit();
	}
    public boolean isLockPattenValid(){
        String lockPattern = sharedPref.getString(SettingsManager.LOCK_KEY, null);
        if (lockPattern == null) {
            return false;
        }
        if (lockPattern.equals("")) {
            return false;
        }
        return  isGestureLockEnabled();
    }
}
