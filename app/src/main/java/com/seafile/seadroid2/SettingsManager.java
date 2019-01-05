package com.seafile.seadroid2;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.text.TextUtils;

import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.util.Utils;

import java.util.Arrays;
import java.util.List;

/**
 * Access the app settings
 */
public final class SettingsManager {
    private static final String DEBUG_TAG = "SettingsManager";

    private static SettingsManager instance;
    private static SharedPreferences settingsSharedPref;
    private static SharedPreferences sharedPref;
    private static SharedPreferences.Editor editor;

    private SettingsManager() {
        if (SeadroidApplication.getAppContext() != null) {
            settingsSharedPref = PreferenceManager.getDefaultSharedPreferences(SeadroidApplication.getAppContext());
            sharedPref = SeadroidApplication.getAppContext().getSharedPreferences(AccountManager.SHARED_PREF_NAME, Context.MODE_PRIVATE);
            editor = sharedPref.edit();
        }
    }


    // Account
    public static final String SETTINGS_ACCOUNT_INFO_KEY = "account_info_user_key";
    public static final String SETTINGS_ACCOUNT_SPACE_KEY = "account_info_space_key";
    public static final String SETTINGS_ACCOUNT_SIGN_OUT_KEY = "account_sign_out_key";

    // privacy category
    public static final String PRIVACY_CATEGORY_KEY = "category_privacy_key";

    // Client side encryption
    public static final String CLIENT_ENC_SWITCH_KEY = "client_encrypt_switch_key";
    public static final String CLEAR_PASSOWR_SWITCH_KEY = "clear_password_switch_key";
    public static final String AUTO_CLEAR_PASSOWR_SWITCH_KEY = "auto_clear_password_switch_key";

    // Gesture Lock
    public static final String GESTURE_LOCK_SWITCH_KEY = "gesture_lock_switch_key";
    public static final String GESTURE_LOCK_KEY = "gesture_lock_key";
    public static final int GESTURE_LOCK_REQUEST = 1;

    // Camera upload
    public static final String PKG = "com.seafile.seadroid2";

    public static final String SHARED_PREF_CONTACTS_UPLOAD_REPO_ID = PKG + ".contacts.repoid";
    public static final String SHARED_PREF_CONTACTS_UPLOAD_REPO_NAME = PKG + ".contacts.repoName";

    public static final String SHARED_PREF_STORAGE_DIR = PKG + ".storageId";

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
    public static final String CAMERA_UPLOAD_BUCKETS_KEY = "camera_upload_buckets_key";
    public static final String CAMERA_UPLOAD_CATEGORY_KEY = "category_camera_upload_key";
    public static final String CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY = "camera_upload_buckets_switch_key";
    public static final String SHARED_PREF_CAMERA_UPLOAD_BUCKETS = PKG + ".camera.buckets";
    //contacts
    public static final String CONTACTS_UPLOAD_CATEGORY_KEY = "category_contacts_upload_key";
    public static final String CONTACTS_UPLOAD_SWITCH_KEY = "contacts_upload_switch_key";
    public static final String SETTINGS_ABOUT_VERSION_KEY = "settings_about_version_key";
    public static final String SETTINGS_ABOUT_AUTHOR_KEY = "settings_about_author_key";
    public static final String CONTACTS_UPLOAD_REPO_KEY = "contacts_upload_repo_key";
    public static final String CONTACTS_UPLOAD_REPO_TIME_KEY = "contacts_upload_repo_time_key";
    public static final String CONTACTS_UPLOAD_REPO_BACKUP_KEY = "contacts_upload_repo_backup_key";
    public static final String CONTACTS_UPLOAD_REPO_RECOVERY_KEY = "contacts_upload_repo_recovery_key";

    // Cache
    public static final String SETTINGS_CACHE_CATEGORY_KEY = "settings_cache_key";
    public static final String SETTINGS_CACHE_SIZE_KEY = "settings_cache_info_key";
    public static final String SETTINGS_CLEAR_CACHE_KEY = "settings_clear_cache_key";
    public static final String SETTINGS_CACHE_DIR_KEY = "settings_cache_location_key";

    // Sort files
    public static final String SORT_FILES_TYPE = "sort_files_type";
    public static final String SORT_FILES_ORDER = "sort_files_order";

    public static long lock_timestamp = 0;
    public static final long LOCK_EXPIRATION_MSECS = 5 * 60 * 1000;

    public static SettingsManager instance() {
        if (instance == null) {
            synchronized (SettingsManager.class) {
                if (instance == null) {
                    instance = new SettingsManager();
                }
            }
        }

        if (settingsSharedPref == null) {
            settingsSharedPref = PreferenceManager.getDefaultSharedPreferences(SeadroidApplication.getAppContext());
        }
        if (sharedPref == null) {
            sharedPref = SeadroidApplication.getAppContext().getSharedPreferences(AccountManager.SHARED_PREF_NAME, Context.MODE_PRIVATE);
            editor = sharedPref.edit();
        }
        return instance;
    }


    public void registerSharedPreferencesListener(SharedPreferences.OnSharedPreferenceChangeListener listener) {
        settingsSharedPref.registerOnSharedPreferenceChangeListener(listener);
        sharedPref.registerOnSharedPreferenceChangeListener(listener);
    }

    public void unregisterSharedPreferencesListener(SharedPreferences.OnSharedPreferenceChangeListener listener) {
        settingsSharedPref.unregisterOnSharedPreferenceChangeListener(listener);
        sharedPref.unregisterOnSharedPreferenceChangeListener(listener);
    }

    /**
     * Client side encryption only support for encrypted library
     */
    public void setupEncrypt(boolean enable) {
        settingsSharedPref.edit().putBoolean(CLIENT_ENC_SWITCH_KEY, enable)
                .commit();
    }

    /**
     * Whether the user has enabled client side encryption
     */
    public boolean isEncryptEnabled() {
        return settingsSharedPref.getBoolean(CLIENT_ENC_SWITCH_KEY, false);
    }

    /**
     * Auto clear password
     */
    public void setupPasswordAutoClear(boolean enable) {
        settingsSharedPref.edit().putBoolean(AUTO_CLEAR_PASSOWR_SWITCH_KEY, enable)
                .commit();
    }

    /**
     * Whether the user has enabled password auto clear when logout account
     */
    public boolean isPasswordAutoClearEnabled() {
        return settingsSharedPref.getBoolean(AUTO_CLEAR_PASSOWR_SWITCH_KEY, false);
    }

    public void setupGestureLock() {
        settingsSharedPref.edit().putBoolean(GESTURE_LOCK_SWITCH_KEY, true)
                .commit();
        saveGestureLockTimeStamp();
    }

    /**
     * Whether the user has setup a gesture lock or not
     */
    public boolean isGestureLockEnabled() {
        return settingsSharedPref.getBoolean(GESTURE_LOCK_SWITCH_KEY, false);
    }

    /**
     * For convenience, if the user has given the correct gesture lock, he
     * would not be asked for gesture lock for a short period of time.
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

    public String getContactsUploadRepoName() {
        return sharedPref.getString(SHARED_PREF_CONTACTS_UPLOAD_REPO_NAME, null);
    }

    public void saveCameraUploadRepoInfo(String repoId, String repoName) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, repoId);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, repoName);
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

    public void saveSortFilesPref(int type, int order) {
        editor.putInt(SORT_FILES_TYPE, type).commit();
        editor.putInt(SORT_FILES_ORDER, order).commit();
    }

    public void setCameraUploadBucketList(List<String> list) {
        String s = TextUtils.join(",", list);
        sharedPref.edit().putString(SHARED_PREF_CAMERA_UPLOAD_BUCKETS, s).commit();
    }

    /**
     * @return list of bucket IDs that have been selected for upload. Empty list means "all buckets"
     */
    public List<String> getCameraUploadBucketList() {
        String s = sharedPref.getString(SHARED_PREF_CAMERA_UPLOAD_BUCKETS, "");
        return Arrays.asList(TextUtils.split(s, ","));
    }

    public int getSortFilesTypePref() {
        return sharedPref.getInt(SORT_FILES_TYPE, 0);
    }

    public int getSortFilesOrderPref() {
        return sharedPref.getInt(SORT_FILES_ORDER, 0);
    }

    public String getCameraUploadRepoId() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
    }

    public String getContactsUploadRepoId() {
        return sharedPref.getString(SettingsManager.SHARED_PREF_CONTACTS_UPLOAD_REPO_ID, null);
    }

    public int getStorageDir() {
        return sharedPref.getInt(SHARED_PREF_STORAGE_DIR, Integer.MIN_VALUE);
    }

    public void setStorageDir(int dir) {
        editor.putInt(SHARED_PREF_STORAGE_DIR, dir).commit();
    }

    public void saveContactsUploadRepoInfo(String repoId, String repoName) {
        editor.putString(SHARED_PREF_CONTACTS_UPLOAD_REPO_ID, repoId);
        editor.putString(SHARED_PREF_CONTACTS_UPLOAD_REPO_NAME, repoName);
        editor.commit();
    }
}
