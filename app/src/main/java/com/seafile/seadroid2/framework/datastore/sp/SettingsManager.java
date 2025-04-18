package com.seafile.seadroid2.framework.datastore.sp;

import android.content.Context;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.preferences.Settings;

/**
 * Access the app settings
 */
public final class SettingsManager {

    private static final Context gAppContext = SeadroidApplication.getAppContext();

    // Account
    public static final String SETTINGS_ACCOUNT_INFO_KEY = gAppContext.getString(R.string.key_user_account_info);
    public static final String SETTINGS_ACCOUNT_SPACE_KEY = gAppContext.getString(R.string.key_user_space);
    public static final String SETTINGS_ACCOUNT_SIGN_OUT_KEY = gAppContext.getString(R.string.key_user_sign_out);


    // Client side encryption
    public static final String CLIENT_ENC_SWITCH_KEY = gAppContext.getString(R.string.key_security_client_encrypt_switch);
    public static final String CLEAR_PASSOWR_SWITCH_KEY = gAppContext.getString(R.string.key_security_clear_password);

    // Gesture Lock
    public static final String GESTURE_LOCK_SWITCH_KEY = gAppContext.getString(R.string.key_user_gesture_switch);


    // Camera upload
    public static final String PKG = "com.seafile.seadroid2";

    public static final String SHARED_PREF_STORAGE_DIR = PKG + ".storageId";

    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_ID = PKG + ".camera.repoid";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_NAME = PKG + ".camera.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL = PKG + ".camera.account.email";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_NAME = PKG + ".camera.account.name";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER = PKG + ".camera.account.server";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN = PKG + ".camera.account.token";
    public static final String SHARED_PREF_CAMERA_UPLOAD_BUCKETS = PKG + ".camera.buckets";

    public static final String CAMERA_UPLOAD_SWITCH_KEY = gAppContext.getString(R.string.key_camera_upload_switch);
    public static final String CAMERA_UPLOAD_REPO_KEY = gAppContext.getString(R.string.key_camera_upload_repo_select);
    public static final String CAMERA_UPLOAD_ADVANCED_CATEGORY_KEY = gAppContext.getString(R.string.key_camera_upload_advanced);
    public static final String CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_data_plan_switch);

    public static final String CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_allow_video_switch);
    public static final String CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_buckets_switch);
    public static final String CAMERA_UPLOAD_BUCKETS_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_buckets_select);
    public static final String CAMERA_UPLOAD_STATE = gAppContext.getString(R.string.key_camera_upload_state);
    public static final String CAMERA_BACKUP_LAST_TIME = "camera_backup_last_time";
    public static final String CAMERA_BACKUP_LAST_MEDIA_VERSION = "camera_backup_last_media_version";


    //ABOUT
    public static final String SETTINGS_ABOUT_VERSION_KEY = gAppContext.getString(R.string.key_about_version);
    public static final String SETTINGS_ABOUT_AUTHOR_KEY = gAppContext.getString(R.string.key_about_author);
    public static final String SETTINGS_PRIVACY_POLICY_KEY = gAppContext.getString(R.string.key_about_privacy);

    // Cache
    public static final String SETTINGS_CACHE_SIZE_KEY = gAppContext.getString(R.string.key_cache_info);
    public static final String SETTINGS_CLEAR_CACHE_KEY = gAppContext.getString(R.string.key_cache_clear);
    public static final String SETTINGS_CACHE_DIR_KEY = gAppContext.getString(R.string.key_cache_location);

    //CameraSync
    @Deprecated
    public static final String UPLOAD_COMPLETED_TIME = "upload_completed_time";

    //FolderBackup
    public static final String FOLDER_BACKUP_SWITCH_KEY = gAppContext.getString(R.string.key_folder_backup_switch);
    public static final String FOLDER_AUTOMATIC_BACKUP_SWITCH_KEY = gAppContext.getString(R.string.key_folder_automatic_backup_switch);
    public static final String FOLDER_BACKUP_ALLOW_DATA_PLAN_SWITCH_KEY = gAppContext.getString(R.string.key_folder_backup_data_plan);
    public static final String FOLDER_BACKUP_NETWORK_MODE = gAppContext.getString(R.string.key_folder_backup_network_mode);
    public static final String FOLDER_BACKUP_MODE = "folder_backup_mode";
    public static final String FOLDER_BACKUP_LIBRARY_KEY = gAppContext.getString(R.string.key_folder_backup_repo_select);
    public static final String FOLDERS_BACKUP_SELECTED_PATH_KEY = gAppContext.getString(R.string.key_folder_backup_folder_select);
    public static final String FOLDER_BACKUP_STATE = gAppContext.getString(R.string.key_folder_backup_state);

    public static final String FOLDER_BACKUP_PATHS = "folder_backup_paths";

    /**
     * The last time the folder backup service was executed
     */
    public static final String FOLDER_BACKUP_LAST_TIME = "folder_backup_last_time";

    /**
     * The last sync time for each backup folder
     */
    public static final String FOLDER_BACKUP_LAST_TIME_PREFIX = "folder_backup_last_time_";

    /**
     * Is it necessary to filter hidden files when the folder backup service is turned on
     */
    public static final String FOLDER_BACKUP_SKIP_HIDDEN_FILES = "folder_backup_filtering_hidden_files";


    //
    public static final String PRIVACY_POLICY_CONFIRMED = "privacy_policy_confirmed";

    public static final long DECRYPTION_EXPIRATION_TIME = 1000 * 60 * 60;// 1h

    public static final int REPO_ENC_VERSION = 2;
}
