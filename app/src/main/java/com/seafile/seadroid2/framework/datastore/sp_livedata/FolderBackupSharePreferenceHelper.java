package com.seafile.seadroid2.framework.datastore.sp_livedata;

import android.content.SharedPreferences;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.enums.NetworkMode;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

public class FolderBackupSharePreferenceHelper {
    public static boolean isFolderBackupEnable() {
        boolean isEnable = FolderBackupSharePreferenceHelper.readBackupSwitch();
        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        List<String> pathList = FolderBackupSharePreferenceHelper.readBackupPathsAsList();

        return (isEnable && !CollectionUtils.isEmpty(pathList) && repoConfig != null);
    }

    public static void writeBackupSwitch(boolean isChecked) {
        if (Settings.FOLDER_BACKUP_SWITCH == null) {
            return;
        }
        Settings.FOLDER_BACKUP_SWITCH.putValue(isChecked);
    }

    public static boolean readBackupSwitch() {
        if (Settings.FOLDER_BACKUP_SWITCH == null) {
            return false;
        }
        return Settings.FOLDER_BACKUP_SWITCH.queryValue();
    }

    public static long readLastScanTime() {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return 0L;
        }
        return sp.getLong(SettingsManager.FOLDER_BACKUP_LAST_TIME, 0);
    }

    public static void writeLastScanTime(long time) {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp != null) {
            sp.edit().putLong(SettingsManager.FOLDER_BACKUP_LAST_TIME, time).apply();
        }
    }

    public static void resetLastScanTime() {
        writeLastScanTime(0L);
    }

    //"WIFI" or "WIFI_AND_MOBILE"
    public static void writeNetworkMode(NetworkMode network) {
        if (Settings.FOLDER_BACKUP_NETWORK_MODE == null) {
            return;
        }
        Settings.FOLDER_BACKUP_NETWORK_MODE.putValue(network);
    }

    public static boolean readDataPlanAllowed() {
        if (Settings.FOLDER_BACKUP_NETWORK_MODE == null) {
            return false;
        }

        NetworkMode n = Settings.FOLDER_BACKUP_NETWORK_MODE.queryValue();
        return NetworkMode.WIFI_AND_MOBILE == n;
    }

    public static void writeRepoConfig(RepoConfig repoConfig) {
        if (Settings.FOLDER_BACKUP_SELECTED_REPO == null) {
            return;
        }

        if (repoConfig == null) {
            Settings.FOLDER_BACKUP_SELECTED_REPO.remove();
        } else {
            String confStr = GsonUtils.toJson(repoConfig);
            Settings.FOLDER_BACKUP_SELECTED_REPO.putValue(confStr);
        }
    }

    @Nullable
    public static RepoConfig readRepoConfig() {
        if (Settings.FOLDER_BACKUP_SELECTED_REPO == null) {
            return null;
        }

        String confStr = Settings.FOLDER_BACKUP_SELECTED_REPO.queryValue();
        if (TextUtils.isEmpty(confStr)) {
            return null;
        }

        return GsonUtils.fromJson(confStr, RepoConfig.class);
    }

    public static void writeBackupPathsAsString(List<String> paths) {
        if (Settings.FOLDER_BACKUP_SELECTED_FOLDERS == null) {
            return;
        }

        if (CollectionUtils.isEmpty(paths)) {
            Settings.FOLDER_BACKUP_SELECTED_FOLDERS.remove();
        } else {
            String confStr = GsonUtils.toJson(paths);
            Settings.FOLDER_BACKUP_SELECTED_FOLDERS.putValue(confStr);
        }
    }

    @Nullable
    public static String readBackupPathStr() {
        if (Settings.FOLDER_BACKUP_SELECTED_FOLDERS == null) {
            return null;
        }

        return Settings.FOLDER_BACKUP_SELECTED_FOLDERS.queryValue();
    }

    @NonNull
    public static List<String> readBackupPathsAsList() {
        if (Settings.FOLDER_BACKUP_SELECTED_FOLDERS == null) {
            return CollectionUtils.newArrayList();
        }

        String confStr = Settings.FOLDER_BACKUP_SELECTED_FOLDERS.queryValue();
        if (TextUtils.isEmpty(confStr) || "[]".equals(confStr) || "null".equals(confStr) || "\"\"".equals(confStr) || "\"null\"".equals(confStr) || "{}".equals(confStr)) {
            return CollectionUtils.newArrayList();
        }

        Type listType = new TypeToken<List<String>>() {
        }.getType();

        List<String> list = GsonUtils.fromJson(confStr, listType);
        return CollectionUtils.isEmpty(list) ? CollectionUtils.newArrayList() : list;
    }

    @Nullable
    public static Long readLastScanTimeForPath(String absPath) {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return null;
        }

        String k = SettingsManager.FOLDER_BACKUP_LAST_TIME_PREFIX + EncryptUtils.encryptMD5ToString(absPath);
        return sp.getLong(k, 0L);
    }

    public static void writeLastScanTimeForPath(String absPath) {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return;
        }

        String k = SettingsManager.FOLDER_BACKUP_LAST_TIME_PREFIX + EncryptUtils.encryptMD5ToString(absPath);
        sp.edit().putLong(k, System.currentTimeMillis()).apply();
    }

    public static void clearLastScanTimeForAllPath() {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return;
        }

        Map<String, ?> m = sp.getAll();
        m.forEach(new BiConsumer<String, Object>() {
            @Override
            public void accept(String key, Object o) {
                if (key.startsWith(SettingsManager.FOLDER_BACKUP_LAST_TIME_PREFIX)) {
                    sp.edit().remove(key).apply();
                }
            }
        });
    }

    public static void clearLastScanTimeForPath(String absPath) {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return;
        }

        String k = SettingsManager.FOLDER_BACKUP_LAST_TIME_PREFIX + EncryptUtils.encryptMD5ToString(absPath);
        sp.edit().remove(k).apply();
    }

    public static void writeSkipHiddenFiles(boolean isSkip) {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return;
        }

        sp.edit().putBoolean(SettingsManager.FOLDER_BACKUP_SKIP_HIDDEN_FILES, isSkip).apply();
    }

    /**
     * Is it necessary to filter hidden files when the folder backup service is turned on
     */
    public static boolean isFolderBackupSkipHiddenFiles() {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return true;
        }

        return sp.getBoolean(SettingsManager.FOLDER_BACKUP_SKIP_HIDDEN_FILES, true);
    }
}
