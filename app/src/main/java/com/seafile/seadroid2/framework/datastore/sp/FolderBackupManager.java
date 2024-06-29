package com.seafile.seadroid2.framework.datastore.sp;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.lang.reflect.Type;
import java.util.List;

public class FolderBackupManager {
//    private static final Context _appContext = SeadroidApplication.getAppContext();
//
//    //FolderBackup
//    public static final String KEY_FOLDER_BACKUP_SWITCH = _appContext.getString(R.string.key_folder_backup_switch);
//    public static final String KEY_FOLDER_BACKUP_NETWORK_MODE = _appContext.getString(R.string.key_folder_backup_network_mode);
//    public static final String KEY_FOLDER_BACKUP_LIBRARY = _appContext.getString(R.string.key_folder_backup_repo_select);
//    public static final String KEY_SELECTED_BACKUP_FOLDERS = _appContext.getString(R.string.key_folder_backup_folder_select);
//    public static final String KEY_FOLDER_BACKUP_STATE = _appContext.getString(R.string.key_folder_backup_state);
//
//    public static final String FOLDER_BACKUP_JUMP_HIDDEN_FILES = "folder_backup_filtering_hidden_files";


    private static String currentAccountSignature = null;

    public static void setCurrentAccount(String currentAccountSignature) {
        FolderBackupManager.currentAccountSignature = currentAccountSignature;
    }

    public static void resetUserInstance() {
        FolderBackupManager.currentAccountSignature = null;
    }

    public static String getCurrentAccount() {
        if (currentAccountSignature == null) {
            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account == null) {
                throw new IllegalArgumentException("account is null.");
            }

            currentAccountSignature = account.getSignature();
        }
        return currentAccountSignature;
    }

    public static void clearAccountWhenLoginStatusChanged() {
        currentAccountSignature = null;
    }

    public static void writeBackupSwitch(boolean isChecked) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeBoolean(SettingsManager.FOLDER_BACKUP_SWITCH_KEY, isChecked);
    }

    public static boolean readBackupSwitch() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readBoolean(SettingsManager.FOLDER_BACKUP_SWITCH_KEY);
    }

    public static long readLastScanTime() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readLong(SettingsManager.FOLDER_BACKUP_LAST_TIME);
    }

    public static void writeLastScanTime(long time) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeLong(SettingsManager.FOLDER_BACKUP_LAST_TIME, time);
    }

    //"WIFI" or "WIFI_AND_MOBILE"
    public static void writeNetworkMode(String network) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeString(SettingsManager.FOLDER_BACKUP_NETWORK_MODE, network);
    }

    public static String readNetworkMode() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readString(SettingsManager.FOLDER_BACKUP_NETWORK_MODE, "WIFI");
    }

    public static boolean readDataPlanAllowed() {
        String n = readNetworkMode();
        return "WIFI_AND_MOBILE".equals(n);
    }

    public static void writeRepoConfig(RepoConfig repoConfig) {
        String confStr = GsonUtils.toJson(repoConfig);

        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeString(SettingsManager.FOLDER_BACKUP_LIBRARY_KEY, confStr);
    }


    @Nullable
    public static RepoConfig readRepoConfig() {
        String confStr = DataStoreManager.getInstanceByUser(getCurrentAccount()).readString(SettingsManager.FOLDER_BACKUP_LIBRARY_KEY);
        if (TextUtils.isEmpty(confStr)) {
            return null;
        }

        return GsonUtils.fromJson(confStr, RepoConfig.class);
    }

    public static void writeBackupPaths(List<String> paths) {
        String confStr;
        if (CollectionUtils.isEmpty(paths)) {
            confStr = "";
        } else {
            confStr = GsonUtils.toJson(paths);
        }

        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeString(SettingsManager.FOLDERS_BACKUP_SELECTED_PATH_KEY, confStr);
    }

    public static String readBackupPathStr() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readString(SettingsManager.FOLDERS_BACKUP_SELECTED_PATH_KEY);
    }

    public static List<String> readBackupPaths() {
        String confStr = DataStoreManager.getInstanceByUser(getCurrentAccount()).readString(SettingsManager.FOLDERS_BACKUP_SELECTED_PATH_KEY);
        if (TextUtils.isEmpty(confStr) || "[]".equals(confStr) || "null".equals(confStr) || "\"\"".equals(confStr) || "\"null\"".equals(confStr) || "{}".equals(confStr)) {
            return CollectionUtils.newArrayList();
        }

        Type listType = new TypeToken<List<String>>() {
        }.getType();

        List<String> list = GsonUtils.fromJson(confStr, listType);
        return CollectionUtils.isEmpty(list) ? CollectionUtils.newArrayList() : list;
    }

    public static long readBackupPathLastScanTime(String absPath) {
        String k = SettingsManager.FOLDER_BACKUP_LAST_TIME_PREFIX + EncryptUtils.encryptMD5ToString(absPath);
        return DataStoreManager
                .getInstanceByUser(getCurrentAccount())
                .readLong(k);
    }

    public static void writeBackupPathLastScanTime(String absPath) {
        String k = SettingsManager.FOLDER_BACKUP_LAST_TIME_PREFIX + EncryptUtils.encryptMD5ToString(absPath);

        DataStoreManager
                .getInstanceByUser(getCurrentAccount())
                .writeLong(k, System.currentTimeMillis());
    }

    public static void clearBackupPathLastScanTime(String absPath) {
        String k = SettingsManager.FOLDER_BACKUP_LAST_TIME_PREFIX + EncryptUtils.encryptMD5ToString(absPath);

        DataStoreManager
                .getInstanceByUser(getCurrentAccount())
                .removeByKey(k);
    }

    public static void writeSkipHiddenFiles(boolean isSkip) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeBoolean(SettingsManager.FOLDER_BACKUP_SKIP_HIDDEN_FILES, isSkip);
    }

    /**
     * Is it necessary to filter hidden files when the folder backup service is turned on
     */
    public static boolean isFolderBackupSkipHiddenFiles() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readAndSetBooleanWhenNotExists(SettingsManager.FOLDER_BACKUP_SKIP_HIDDEN_FILES, true);
    }
}
