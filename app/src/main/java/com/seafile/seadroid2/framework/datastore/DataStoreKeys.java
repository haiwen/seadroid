package com.seafile.seadroid2.framework.datastore;

public class DataStoreKeys {
    public static final String SEPARATOR = "::::";

    //data store
//    public static final String DS_REPO_DIR_MAPPING = "data_store_repo_dir_mapping";
    public static final String LATEST_ACCOUNT = "latest_account";

    public static final String ACCOUNT_CURRENT_OLD = "com.seafile.seadroid.account_name";
    public static final String KEY_CURRENT_ACCOUNT = "current_account_signature";

    /**
     * When the app version is upgraded to v3.0.0(or v3x), some data must be migrated,
     * such as. CameraUploadDBHelper/FolderBackupDBHelper.
     * This field is used to check if it has been migrated.
     * <p>
     * 0 no
     * 1 yes
     * <p/>
     */
    public static final String DATA_IS_MIGRATION = "data_is_migrated_when_app_version_is_v3x";

    /**
     * When the app version is upgraded to v3.0.3, some data must be migrated,
     * such as. FolderBackupManager/AlbumBackupManager/GestureLockManager/SettingsManager.
     * This field is used to check if it has been migrated.
     * <p>
     * 0 no
     * 1 yes
     * <p/>
     */
    public static final String DATA_IS_MIGRATED_WHEN_APP_IS_V303 = "data_is_migrated_when_app_version_is_v303";


    public static final String KEY_DARK_MODE = "key_dark_mode";

    public static final String KEY_SERVER_CERT_INFO = "key_server_cert_info";

    public static final String KEY_NAV_CONTEXT_STACK = "key_nav_context_stack";
}