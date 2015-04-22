package com.seafile.seadroid2;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;

/**
 * Access the app settings
 */
public final class SettingsManager {
    private static final String DEBUG_TAG = "SettingsManager";

    // Global variables
    private SharedPreferences sharedPref = SeadroidApplication.getAppContext()
            .getSharedPreferences(AccountManager.SHARED_PREF_NAME, Context.MODE_PRIVATE);

    private SharedPreferences.Editor editor = sharedPref.edit();
    private static SettingsManager instance;

    private SettingsManager() {
    }

    private SharedPreferences settingsSharedPref = PreferenceManager
            .getDefaultSharedPreferences(SeadroidApplication.getAppContext());

    // Account
    public static final String SETTINGS_ACCOUNT_INFO_KEY = "account_info_user_key";
    public static final String SETTINGS_ACCOUNT_SPACE_KEY = "account_info_space_key";
    public static final String SETTINGS_ACCOUNT_SIGN_OUT_KEY = "account_sign_out_key";

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
    public static final String CAMERA_UPLOAD_SWITCH_KEY = "camera_upload_switch_key";
    public static final String CAMERA_UPLOAD_REPO_KEY = "camera_upload_repo_key";
    public static final String CAMERA_UPLOAD_ADVANCED_SCREEN_KEY = "screen_camera_upload_advanced_feature";
    public static final String CAMERA_UPLOAD_ADVANCED_CATEGORY_KEY = "category_camera_upload_advanced_key";
    public static final String CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY = "allow_data_plan_switch_key";
    public static final String CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY = "allow_videos_upload_switch_key";
    public static final String CAMERA_UPLOAD_DIRECTORY_KEY = "camera_upload_directory_key";
    public static final String CAMERA_UPLOAD_CATEGORY_KEY = "category_camera_upload_key";
    public static final String CAMERA_UPLOAD_CUSTOM_DIRECTORIES_KEY = "camera_upload_directory_switch_key";
    public static final String CAMERA_UPLOAD_CUSTOM_DIRECTORIES_PATH = "camera_upload_directory_path";
    public static final int CHOOSE_CAMERA_UPLOAD_REQUEST = 2;

    // About tab
    public static final String SETTINGS_ABOUT_VERSION_KEY = "settings_about_version_key";
    public static final String SETTINGS_ABOUT_AUTHOR_KEY = "settings_about_author_key";

    // Cache
    public static final String SETTINGS_CACHE_SIZE_KEY = "settings_cache_info_key";
    public static final String SETTINGS_CLEAR_CACHE_KEY = "settings_clear_cache_key";

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

    /**
     * Whether the user has setup a gesture lock or not
     *
     */
    public boolean isGestureLockEnabled() {
        return settingsSharedPref.getBoolean(GESTURE_LOCK_SWITCH_KEY, false);
    }

    /**
     * For convenience, if the user has given the correct gesture lock, he
     * would not be asked for gesture lock for a short period of time.
     *
     */
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

    public void clearCameraUploadInfo() {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
        editor.commit();
    }

    public boolean checkCameraUploadNetworkAvailable() {
        if (!Utils.isNetworkOn()) {
            return false;
        }
        // user does not allow mobile connections
        if (!Utils.isWiFiOn() && !isDataPlanAllowed()) {
            return false;
        }
        // Wi-Fi or 2G/3G/4G connections available
        return true;
    }

    public void saveCameraUploadEnabled(boolean isEnabled) {
        settingsSharedPref.edit().putBoolean(CAMERA_UPLOAD_SWITCH_KEY, isEnabled).commit();
    }

    public boolean isCameraUploadEnabled() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_SWITCH_KEY, false);
    }

    public boolean isDataPlanAllowed() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY, false);
    }

    public boolean isVideosUploadAllowed() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY, false);
    }

    public void saveDataPlanAllowed(boolean isAllowed) {
        settingsSharedPref.edit().putBoolean(CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY, isAllowed).commit();
    }

    public void saveVideosAllowed(boolean isVideosUploadAllowed) {
        settingsSharedPref.edit().putBoolean(CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY, isVideosUploadAllowed).commit();
    }

    public void saveCustomScanDir(boolean isCustom) {
        settingsSharedPref.edit().putBoolean(CAMERA_UPLOAD_CUSTOM_DIRECTORIES_KEY, isCustom).commit();
    }

    public void saveLocalDirPath(String path) {
        editor.putString(CAMERA_UPLOAD_CUSTOM_DIRECTORIES_PATH, path).commit();
    }

    public String getLocalDirPath() {
        return sharedPref.getString(CAMERA_UPLOAD_CUSTOM_DIRECTORIES_PATH, null);
    }

    public boolean isCustomScanDir() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_CUSTOM_DIRECTORIES_KEY, false);
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

    /**
     * get current login Account instance
     *
     * @return Account if has, otherwise, returns null.
     */
    public Account getCurrentAccount() {
        AccountManager accountMgr = new AccountManager(
                SeadroidApplication.getAppContext());
        return accountMgr.getCurrentAccount();
    }

    public void delCachesByActSignature(Account account) {
        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        dbHelper.delCachesBySignature(account);
    }
}
