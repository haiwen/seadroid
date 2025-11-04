package com.seafile.seadroid2.preferences;

import android.content.SharedPreferences;
import android.content.res.Resources;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.enums.SortBy;
import com.seafile.seadroid2.enums.NetworkMode;
import com.seafile.seadroid2.enums.NightMode;
import com.seafile.seadroid2.preferences.livedata.BooleanSettingLiveData;
import com.seafile.seadroid2.preferences.livedata.EnumSettingLiveData;
import com.seafile.seadroid2.preferences.livedata.LongSettingLiveData;
import com.seafile.seadroid2.preferences.livedata.StringSettingLiveData;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Settings {
    public Settings() {
        throw new IllegalStateException("Don't instantiate this class");
    }

    private static Account _account;
    private static SharedPreferences sharedPreferences;

    //////////////////
    /// app settings
    //////////////////
    public static final SettingsLiveData<FileViewType> FILE_LIST_VIEW_TYPE = new EnumSettingLiveData<>(FileViewType.class, R.string.pref_key_file_list_view_type, R.string.pref_default_value_file_list_view_type);
    public static final SettingsLiveData<SortBy> FILE_LIST_SORT_BY = new EnumSettingLiveData<>(SortBy.class, R.string.pref_key_file_list_sort_options, R.string.pref_default_value_file_list_options);
    public static final SettingsLiveData<Boolean> FILE_LIST_SORT_ASCENDING = new BooleanSettingLiveData(R.string.pref_key_file_list_sort_ascending);
    public static final SettingsLiveData<Boolean> FILE_LIST_SORT_FOLDER_FIRST = new BooleanSettingLiveData(R.string.pref_key_file_list_sort_folder_first, R.bool.pref_default_true);

    //gesture
    public static final SettingsLiveData<Boolean> SETTINGS_GESTURE = new BooleanSettingLiveData(R.string.pref_key_settings_gesture_lock, R.bool.pref_default_true);
    public static final SettingsLiveData<Long> SETTINGS_GESTURE_LOCK_TIMESTAMP = new LongSettingLiveData(R.string.pref_key_settings_gesture_lock_timestamp, R.string.pref_default_value_key_gesture_lock_timestamp);

    //////////////////
    /// user settings
    //////////////////
    public static SettingsLiveData<String> USER_INFO;
    public static SettingsLiveData<String> SPACE_INFO;
    public static SettingsLiveData<String> USER_SERVER_INFO;
    public static SettingsLiveData<NightMode> NIGHT_MODE;

    @Deprecated
    public static SettingsLiveData<Boolean> USER_GESTURE_LOCK_SWITCH;
//    public static SettingsLiveData<Long> USER_GESTURE_LOCK_TIMESTAMP;


    @Deprecated
    public static SettingsLiveData<Boolean> CLIENT_ENCRYPT_SWITCH;

    public static SettingsLiveData<Boolean> BACKGROUND_BACKUP_SWITCH;


    //album backup
    public static SettingsLiveData<Boolean> ALBUM_BACKUP_SWITCH;
    public static SettingsLiveData<String> ALBUM_BACKUP_SELECTED_REPO;
    public static SettingsLiveData<String> ALBUM_BACKUP_STATE;

    public static SettingsLiveData<Boolean> ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH;
    public static SettingsLiveData<Boolean> ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH;
    public static SettingsLiveData<Boolean> ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH;
    public static SettingsLiveData<String> ALBUM_BACKUP_ADVANCE_BUCKETS_SELECT;
    public static SettingsLiveData<String> ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING;//notice this field

    //folder backup
    public static SettingsLiveData<Boolean> FOLDER_BACKUP_SWITCH;
    public static SettingsLiveData<Boolean> FOLDER_BACKUP_SYNC_HIDDEN_FILES;
    public static SettingsLiveData<NetworkMode> FOLDER_BACKUP_NETWORK_MODE;
    public static SettingsLiveData<String> FOLDER_BACKUP_SELECTED_REPO;
    public static SettingsLiveData<String> FOLDER_BACKUP_SELECTED_FOLDERS;
    public static SettingsLiveData<String> FOLDER_BACKUP_STATE;

    public static SettingsLiveData<String> TRANSFER_DOWNLOAD_STATE;
    public static SettingsLiveData<String> TRANSFER_UPLOAD_STATE;
    //cache
    public static SettingsLiveData<String> CACHE_SIZE;


    private static final List<SettingsLiveData<?>> REGISTER_LIST = new ArrayList<>();

    @Nullable
    public static SharedPreferences getCurrentAccountSharedPreferences() {
        if (sharedPreferences == null) {
            if (_account == null) {
                return null;
            }
            sharedPreferences = SharedPreferencesHelper.getSharedPreferences(_account.getEncryptSignature());
        }
        return sharedPreferences;
    }

    @NonNull
    public static SharedPreferences getSpecialUserSharedPreferences(Account specialAccount) {
        return getSpecialUserSharedPreferences(specialAccount.getEncryptSignature());
    }

    @NonNull
    public static SharedPreferences getSpecialUserSharedPreferences(String encryptSignature) {
        return SharedPreferencesHelper.getSharedPreferences(encryptSignature);
    }

    @NonNull
    public static SharedPreferences getCommonPreferences() {
        return SharedPreferencesHelper.getSharedPreferences(null);
    }

    /**
     * Initialization is required before use
     */
    public static void initUserSettings() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        initUserSettings(account);
    }

    public static void initUserSettings(Account account) {
        if (account == null) {
            return;
        }

        if (Objects.equals(_account, account)) {
            return;
        }

        _account = account;

        unregisterIfNotNull();

        //clear old
        REGISTER_LIST.clear();

        sharedPreferences = SharedPreferencesHelper.getSharedPreferences(_account.getEncryptSignature());

        //user
        USER_INFO = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_user_info, R.string.settings_account_info_load_data);
        SPACE_INFO = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_user_space, R.string.settings_account_info_load_data);
        USER_SERVER_INFO = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_user_server, Resources.ID_NULL);
        NIGHT_MODE = new EnumSettingLiveData<>(NightMode.class, _account.getEncryptSignature(), R.string.pref_key_night_mode, R.string.pref_default_value_night_mode);
//        CLIENT_ENCRYPT_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_security_client_encrypt);

//        USER_GESTURE_LOCK_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_gesture_lock);
//        USER_GESTURE_LOCK_TIMESTAMP = new LongSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_gesture_lock_timestamp, R.string.pref_default_value_key_gesture_lock_timestamp);

        BACKGROUND_BACKUP_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(),R.string.pref_key_backup_settings_turn_on_background_switch);
        //album backup advance
        ALBUM_BACKUP_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_switch);
        ALBUM_BACKUP_SELECTED_REPO = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_repo_select, Resources.ID_NULL);
        ALBUM_BACKUP_STATE = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_state, R.string.done);
        //album backup advance
        ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_advanced_data_plan_switch);
        ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_advanced_allow_video_switch);
        ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_advanced_buckets_switch);
        ALBUM_BACKUP_ADVANCE_BUCKETS_SELECT = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_advanced_buckets_select, Resources.ID_NULL);
        ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_album_backup_advanced_selected_buckets_string, Resources.ID_NULL);

        //
        FOLDER_BACKUP_SWITCH = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_folder_backup_switch);
        FOLDER_BACKUP_SYNC_HIDDEN_FILES = new BooleanSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_folder_backup_sync_hidden_files);
        FOLDER_BACKUP_STATE = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_folder_backup_state, R.string.done);
        FOLDER_BACKUP_SELECTED_REPO = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_folder_backup_repo_select, Resources.ID_NULL);
        FOLDER_BACKUP_SELECTED_FOLDERS = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_folder_backup_folder_select, Resources.ID_NULL);
        FOLDER_BACKUP_NETWORK_MODE = new EnumSettingLiveData<>(NetworkMode.class, _account.getEncryptSignature(), R.string.pref_key_folder_backup_network_mode, R.string.pref_key_default_value_folder_backup_network_mode);

        TRANSFER_DOWNLOAD_STATE = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_transfer_download_state, R.string.transfer_default_state);
        TRANSFER_UPLOAD_STATE = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_transfer_upload_state, R.string.transfer_default_state);


        //cache
        CACHE_SIZE = new StringSettingLiveData(_account.getEncryptSignature(), R.string.pref_key_cache_info, R.string.settings_account_info_load_data);


        REGISTER_LIST.add(USER_INFO);
        REGISTER_LIST.add(SPACE_INFO);
        REGISTER_LIST.add(USER_SERVER_INFO);
        REGISTER_LIST.add(NIGHT_MODE);
//        REGISTER_LIST.add(USER_GESTURE_LOCK_SWITCH);
//        REGISTER_LIST.add(USER_GESTURE_LOCK_TIMESTAMP);
//        REGISTER_LIST.add(CLIENT_ENCRYPT_SWITCH);
        REGISTER_LIST.add(ALBUM_BACKUP_SWITCH);
        REGISTER_LIST.add(ALBUM_BACKUP_SELECTED_REPO);
        REGISTER_LIST.add(ALBUM_BACKUP_STATE);
        REGISTER_LIST.add(ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH);
        REGISTER_LIST.add(ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH);
        REGISTER_LIST.add(ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH);
        REGISTER_LIST.add(ALBUM_BACKUP_ADVANCE_BUCKETS_SELECT);
        REGISTER_LIST.add(ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING);
        REGISTER_LIST.add(FOLDER_BACKUP_SWITCH);
        REGISTER_LIST.add(FOLDER_BACKUP_SYNC_HIDDEN_FILES);
        REGISTER_LIST.add(FOLDER_BACKUP_STATE);
        REGISTER_LIST.add(FOLDER_BACKUP_NETWORK_MODE);
        REGISTER_LIST.add(FOLDER_BACKUP_SELECTED_REPO);
        REGISTER_LIST.add(FOLDER_BACKUP_SELECTED_FOLDERS);
        REGISTER_LIST.add(TRANSFER_DOWNLOAD_STATE);
        REGISTER_LIST.add(TRANSFER_UPLOAD_STATE);

        REGISTER_LIST.add(CACHE_SIZE);
    }

    private static void unregisterIfNotNull() {
        if (!REGISTER_LIST.isEmpty()) {
            for (SettingsLiveData<?> settingsLiveData : REGISTER_LIST) {
                settingsLiveData.unregister();
            }
        }
    }
}
