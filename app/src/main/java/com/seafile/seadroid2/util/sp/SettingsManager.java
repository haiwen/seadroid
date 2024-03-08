package com.seafile.seadroid2.util.sp;

import android.content.SharedPreferences;
import android.text.TextUtils;

import androidx.preference.PreferenceManager;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.util.Utils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Access the app settings
 */
public final class SettingsManager {


    // Account
    public static final String SETTINGS_ACCOUNT_INFO_KEY = "account_info_user_key";
    public static final String SETTINGS_ACCOUNT_SPACE_KEY = "account_info_space_key";
    public static final String SETTINGS_ACCOUNT_SIGN_OUT_KEY = "account_sign_out_key";

    // privacy category
    public static final String PRIVACY_CATEGORY_KEY = "category_privacy_key";

    // Client side encryption
    public static final String CLIENT_ENC_SWITCH_KEY = "client_encrypt_switch_key";
    public static final String CLEAR_PASSOWR_SWITCH_KEY = "clear_password_switch_key";

    // Gesture Lock
    public static final String GESTURE_LOCK_SWITCH_KEY = "gesture_lock_switch_key";
    public static final String GESTURE_LOCK_KEY = "gesture_lock_key";
    public static final int GESTURE_LOCK_REQUEST = 1;

    // Camera upload
    public static final String PKG = "com.seafile.seadroid2";

    public static final String SHARED_PREF_STORAGE_DIR = PKG + ".storageId";

    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_ID = PKG + ".camera.repoid";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_NAME = PKG + ".camera.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL = PKG + ".camera.account.email";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_NAME = PKG + ".camera.account.name";
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

    //ABOUT
    public static final String SETTINGS_ABOUT_CATEGORY_KEY = "settings_section_about_key";
    public static final String SETTINGS_ABOUT_VERSION_KEY = "settings_about_version_key";
    public static final String SETTINGS_ABOUT_AUTHOR_KEY = "settings_about_author_key";
    public static final String SETTINGS_PRIVACY_POLICY_KEY = "settings_privacy_policy_key";
    public static final String CONTACTS_UPLOAD_REPO_KEY = "contacts_upload_repo_key";
    public static final String CONTACTS_UPLOAD_REPO_TIME_KEY = "contacts_upload_repo_time_key";
    public static final String CONTACTS_UPLOAD_REPO_BACKUP_KEY = "contacts_upload_repo_backup_key";
    public static final String CONTACTS_UPLOAD_REPO_RECOVERY_KEY = "contacts_upload_repo_recovery_key";

    // Cache
    public static final String SETTINGS_CACHE_CATEGORY_KEY = "settings_cache_key";
    public static final String SETTINGS_CACHE_SIZE_KEY = "settings_cache_info_key";
    public static final String SETTINGS_CLEAR_CACHE_KEY = "settings_clear_cache_key";
    public static final String SETTINGS_CACHE_DIR_KEY = "settings_cache_location_key";
    public static final String CAMERA_UPLOAD_STATE = "camera_upload_state";

    //CameraSyncStatus
    public static final String WAITING_UPLOAD_NUMBER = "waiting_upload_number";
    public static final String TOTAL_UPLOAD_NUMBER = "total_upload_number";
    public static final String PIC_CHECK_START = "pic_check_start";
    public static final String UPLOAD_COMPLETED_TIME = "upload_completed_time";

    //FolderBackupStatus
    public static final String FOLDER_BACKUP_SWITCH_KEY = "folder_backup_switch_key";
    public static final String FOLDER_BACKUP_ALLOW_DATA_PLAN_SWITCH_KEY = "folder_backup_allow_data_plan_switch_key";
    public static final String FOLDER_AUTOMATIC_BACKUP_SWITCH_KEY = "folder_automatic_backup_switch_key";
    public static final String FOLDER_BACKUP_CATEGORY_KEY = "folder_backup_category_key";
    public static final String FOLDER_BACKUP_MODE = "folder_backup_mode";
    public static final String FOLDER_BACKUP_LIBRARY_KEY = "folder_backup_library_key";
    public static final String SELECTED_BACKUP_FOLDERS_KEY = "selected_backup_folders_key";
    public static final String FOLDER_BACKUP_STATE = "folder_backup_state";

    /**
     * Is it necessary to filter hidden files when the folder backup service is turned on
     */
    public static final String FOLDER_BACKUP_JUMP_HIDDEN_FILES = "folder_backup_filtering_hidden_files";

    public static long lock_timestamp = 0;
    public static final long LOCK_EXPIRATION_MSECS = 5 * 60 * 1000;

    public static final String PRIVACY_POLICY_CONFIRMED = "privacy_policy_confirmed";


    private static SharedPreferences settingsSharedPref;

    private SettingsManager() {
        if (settingsSharedPref == null) {
            settingsSharedPref = PreferenceManager.getDefaultSharedPreferences(SeadroidApplication.getAppContext());
        }
    }

    private static class SingletonHolder {
        private static final SettingsManager instance = new SettingsManager();
    }

    public static SettingsManager getInstance() {
        return SingletonHolder.instance;
    }


    public void registerSharedPreferencesListener(SharedPreferences.OnSharedPreferenceChangeListener listener) {
        settingsSharedPref.registerOnSharedPreferenceChangeListener(listener);
        SPs.registerOnSharedPreferenceChangeListener(listener);
    }

    public void unregisterSharedPreferencesListener(SharedPreferences.OnSharedPreferenceChangeListener listener) {
        settingsSharedPref.unregisterOnSharedPreferenceChangeListener(listener);
        SPs.unregisterOnSharedPreferenceChangeListener(listener);
    }

    /**
     * Client side encryption only support for encrypted library
     */
    public void setupEncrypt(boolean enable) {
        settingsSharedPref.edit().putBoolean(CLIENT_ENC_SWITCH_KEY, enable).apply();
    }

    /**
     * Whether the user has enabled client side encryption
     */
    public boolean isEncryptEnabled() {
        return settingsSharedPref.getBoolean(CLIENT_ENC_SWITCH_KEY, false);
    }

    public void setupGestureLock() {
        settingsSharedPref.edit().putBoolean(GESTURE_LOCK_SWITCH_KEY, true).apply();
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


    public boolean checkCameraUploadNetworkAvailable() {
        if (!NetworkUtils.isConnected()) {
            return false;
        }
        // user does not allow mobile connections
        if (!Utils.isWiFiOn() && !isDataPlanAllowed()) {
            return false;
        }
        // Wi-Fi or 2G/3G/4G connections available
        return true;
    }

    //camera backup
    public void saveCameraUploadRepoInfo(String repoId, String repoName) {
        SPs.put(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, repoId);
        SPs.put(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, repoName);
    }
    public void clearCameraUploadRepoInfo(){
        SPs.remove(SHARED_PREF_CAMERA_UPLOAD_REPO_ID);
        SPs.remove(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME);
        SPs.remove(SHARED_PREF_CAMERA_UPLOAD_BUCKETS);
    }

    public String getCameraUploadRepoId() {
        return SPs.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
    }

    public String getCameraUploadRepoName() {
        return SPs.getString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);
    }

    public boolean isDataPlanAllowed() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY, false);
    }


    public boolean isVideosUploadAllowed() {
        return settingsSharedPref.getBoolean(CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY, false);
    }

    public void saveDataPlanAllowed(boolean isAllowed) {
        settingsSharedPref.edit().putBoolean(CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY, isAllowed).apply();
    }

    public void saveVideosAllowed(boolean isVideosUploadAllowed) {
        settingsSharedPref.edit().putBoolean(CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY, isVideosUploadAllowed).apply();
    }

    public void setCameraUploadBucketList(List<String> list) {
        String s = TextUtils.join(",", list);
        SPs.put(SHARED_PREF_CAMERA_UPLOAD_BUCKETS, s);
    }

    /**
     * @return list of bucket IDs that have been selected for upload. Empty list means "all buckets"
     */
    public List<String> getCameraUploadBucketList() {
        String s = SPs.getString(SHARED_PREF_CAMERA_UPLOAD_BUCKETS, "");
        return Arrays.asList(TextUtils.split(s, ","));
    }


    //folder backup
    public boolean isFolderBackupDataPlanAllowed() {
        return settingsSharedPref.getBoolean(FOLDER_BACKUP_ALLOW_DATA_PLAN_SWITCH_KEY, false);
    }

    public void saveFolderBackupDataPlanAllowed(boolean isAllowed) {
        settingsSharedPref.edit().putBoolean(FOLDER_BACKUP_ALLOW_DATA_PLAN_SWITCH_KEY, isAllowed).apply();
    }

    public void saveFolderAutomaticBackup(boolean isAllowed) {
        settingsSharedPref.edit().putBoolean(FOLDER_AUTOMATIC_BACKUP_SWITCH_KEY, isAllowed).apply();
    }

    public boolean isFolderAutomaticBackup() {
        return settingsSharedPref.getBoolean(FOLDER_AUTOMATIC_BACKUP_SWITCH_KEY, false);
    }



    public int getStorageDir() {
        return SPs.getInt(SHARED_PREF_STORAGE_DIR, Integer.MIN_VALUE);
    }

    public void setStorageDir(int dir) {
        SPs.put(SHARED_PREF_STORAGE_DIR, dir);
    }

    public void saveUploadCompletedTime(String completedTime) {
        SPs.put(UPLOAD_COMPLETED_TIME, completedTime);
    }

    public String getUploadCompletedTime() {
        return SPs.getString(SettingsManager.UPLOAD_COMPLETED_TIME);
    }

    public void savePrivacyPolicyConfirmed(int type) {
        SPs.put(PRIVACY_POLICY_CONFIRMED, type);
    }

    public int getPrivacyPolicyConfirmed() {
        return SPs.getInt(PRIVACY_POLICY_CONFIRMED, 0);
    }


    public void setFolderBackupJumpHiddenFiles(boolean isJump) {
        SPs.put(FOLDER_BACKUP_JUMP_HIDDEN_FILES, isJump);
    }

    /**
     * Is it necessary to filter hidden files when the folder backup service is turned on
     */
    public boolean isFolderBackupJumpHiddenFiles() {
        if (!SPs.containsKey(FOLDER_BACKUP_JUMP_HIDDEN_FILES)) {
            setFolderBackupJumpHiddenFiles(true);
            return true;
        }

        return SPs.getBoolean(SettingsManager.FOLDER_BACKUP_JUMP_HIDDEN_FILES, true);
    }
}
