package com.seafile.seadroid2.framework.datastore.sp_livedata;

import android.content.SharedPreferences;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.Arrays;
import java.util.List;

public class AlbumBackupSharePreferenceHelper {
    public static void writeBackupSwitch(boolean isChecked) {
        if (Settings.ALBUM_BACKUP_SWITCH == null) {
            return;
        }

        Settings.ALBUM_BACKUP_SWITCH.putValue(isChecked);
    }


    public static boolean readBackupSwitch() {
        if (Settings.ALBUM_BACKUP_SWITCH == null) {
            return false;
        }

        return Settings.ALBUM_BACKUP_SWITCH.queryValue();
    }

    public static void writeLastScanTime(long time) {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return;
        }

        sp.edit().putLong(SettingsManager.CAMERA_BACKUP_LAST_TIME, time).apply();
    }

    public static void resetLastScanTime() {
        writeLastScanTime(0L);
    }

    public static long readLastScanTimeMills() {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return 0L;
        }

        return sp.getLong(SettingsManager.CAMERA_BACKUP_LAST_TIME, 0L);
    }

    public static void writeLastMediaVersion(String version) {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return;
        }

        sp.edit().putString(SettingsManager.CAMERA_BACKUP_LAST_MEDIA_VERSION, version).apply();
    }

    @Nullable
    public static String readLastMediaVersion() {
        SharedPreferences sp = Settings.getCurrentAccountSharedPreferences();
        if (sp == null) {
            return null;
        }
        return sp.getString(SettingsManager.CAMERA_BACKUP_LAST_MEDIA_VERSION, null);
    }


    public static void writeRepoConfig(RepoConfig repoConfig) {
        if (Settings.ALBUM_BACKUP_SELECTED_REPO == null) {
            return;
        }

        if (repoConfig == null) {
            Settings.ALBUM_BACKUP_SELECTED_REPO.remove();
        } else {
            String confStr = GsonUtils.toJson(repoConfig);
            Settings.ALBUM_BACKUP_SELECTED_REPO.putValue(confStr);
        }
    }

    @Nullable
    public static RepoConfig readRepoConfig() {
        if (Settings.ALBUM_BACKUP_SELECTED_REPO == null) {
            return null;
        }

        String confStr = Settings.ALBUM_BACKUP_SELECTED_REPO.queryValue();
        if (TextUtils.isEmpty(confStr)) {
            return null;
        }

        return GsonUtils.fromJson(confStr, RepoConfig.class);
    }


    //data plan
    public static void writeAllowDataPlanSwitch(boolean isChecked) {
        if (Settings.ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH == null) {
            return;
        }

        Settings.ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH.putValue(isChecked);
    }

    public static boolean readAllowDataPlanSwitch() {
        if (Settings.ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH == null) {
            return false;
        }

        return Settings.ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH.queryValue();
    }

    //video
    public static void writeAllowVideoSwitch(boolean isChecked) {
        if (Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH == null) {
            return;
        }

        Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH.putValue(isChecked);
    }

    public static boolean readAllowVideoSwitch() {
        if (Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH == null) {
            return false;
        }

        return Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH.queryValue();
    }

    //custom album
    public static void writeCustomAlbumSwitch(boolean isChecked) {
        if (Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH == null) {
            return;
        }

        Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH.putValue(isChecked);
    }

    public static boolean readCustomAlbumSwitch() {
        if (Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH == null) {
            return false;
        }

        return Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH.queryValue();
    }

    public static void writeBucketIds(List<String> paths) {
        if (Settings.ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING == null) {
            return;
        }

        if (CollectionUtils.isEmpty(paths)) {
            Settings.ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING.remove();
        } else {
            String confStr = TextUtils.join(",", paths);
            Settings.ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING.putValue(confStr);
        }
    }

    /**
     * @return list of bucket IDs that have been selected for upload. Empty list means "all buckets"
     */
    @NonNull
    public static List<String> readBucketIds() {
        if (Settings.ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING == null) {
            return CollectionUtils.newArrayList();
        }

        String confStr = Settings.ALBUM_BACKUP_ADVANCE_SELECTED_BUCKETS_STRING.queryValue();
        if (TextUtils.isEmpty(confStr)) {
            return CollectionUtils.newArrayList();
        }
        return Arrays.asList(TextUtils.split(confStr, ","));
    }

}
