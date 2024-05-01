package com.seafile.seadroid2.framework.datastore.sp;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.Arrays;
import java.util.List;

public class AlbumBackupManager {
    private AlbumBackupManager() {

    }

//    private static final Context gAppContext = SeadroidApplication.getAppContext();
//    public static final String CAMERA_UPLOAD_SWITCH_KEY = gAppContext.getString(R.string.key_camera_upload_switch);
//    public static final String CAMERA_UPLOAD_REPO_KEY = gAppContext.getString(R.string.key_camera_upload_repo_select);
//    public static final String CAMERA_UPLOAD_STATE_KEY = gAppContext.getString(R.string.key_camera_upload_state);
//    public static final String CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_data_plan_switch);
//    public static final String CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_allow_video_switch);
//    public static final String CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_buckets_switch);
//    public static final String CAMERA_UPLOAD_BUCKETS_KEY = gAppContext.getString(R.string.key_camera_upload_advanced_buckets_select);
//

    private static Account currentAccount = null;

    public static void setCurrentAccount(Account account) {
        AlbumBackupManager.currentAccount = account;
    }

    public static void resetUserInstance() {
        AlbumBackupManager.currentAccount = null;
    }

    public static String getCurrentAccount() {
        return getInstanceAccount().getSignature();
    }

    public static Account getInstanceAccount() {
        if (currentAccount == null) {
            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account == null) {
                throw new IllegalArgumentException("account is null.");
            }

            currentAccount = account;
        }
        return currentAccount;
    }

    public static void clearAccountWhenLoginStatusChanged() {
        currentAccount = null;
        CameraUploadManager.getInstance().disableCameraUpload();
    }

    public static void writeBackupSwitch(boolean isChecked) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeBoolean(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY, isChecked);
    }

    public static boolean readBackupSwitch() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readBoolean(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY);
    }

    public static long readLastScanTime() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readLong(SettingsManager.CAMERA_BACKUP_LAST_TIME);
    }

    public static void writeLastScanTime(long time) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeLong(SettingsManager.CAMERA_BACKUP_LAST_TIME, time);
    }

    public static void writeRepoConfig(RepoConfig repoConfig) {
        String confStr = GsonUtils.toJson(repoConfig);

        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeString(SettingsManager.CAMERA_UPLOAD_REPO_KEY, confStr);
    }

    public static void clearRepoConfig() {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).removeByKey(SettingsManager.CAMERA_UPLOAD_REPO_KEY);
    }

    @Nullable
    public static RepoConfig readRepoConfig() {
        String confStr = DataStoreManager.getInstanceByUser(getCurrentAccount()).readString(SettingsManager.CAMERA_UPLOAD_REPO_KEY);
        if (TextUtils.isEmpty(confStr)) {
            return null;
        }

        return GsonUtils.fromJson(confStr, RepoConfig.class);
    }

    //data plan
    public static void writeAllowDataPlanSwitch(boolean isChecked) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeBoolean(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY, isChecked);
    }

    public static boolean readAllowDataPlanSwitch() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readBoolean(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY);
    }


    //video
    public static void writeAllowVideoSwitch(boolean isChecked) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeBoolean(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY, isChecked);
    }

    public static boolean readAllowVideoSwitch() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readBoolean(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY);
    }

    //custom album
    public static void writeCustomAlbumSwitch(boolean isChecked) {
        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeBoolean(SettingsManager.CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY, isChecked);
    }

    public static boolean readCustomAlbumSwitch() {
        return DataStoreManager.getInstanceByUser(getCurrentAccount()).readBoolean(SettingsManager.CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY);
    }


    public static void writeBucketIds(List<String> paths) {
        String confStr;
        if (CollectionUtils.isEmpty(paths)) {
            confStr = "";
        } else {
            confStr = TextUtils.join(",", paths);
        }

        DataStoreManager.getInstanceByUser(getCurrentAccount()).writeString(SettingsManager.CAMERA_UPLOAD_BUCKETS_KEY, confStr);
    }

    /**
     * @return list of bucket IDs that have been selected for upload. Empty list means "all buckets"
     */
    public static List<String> readBucketIds() {
        String confStr = DataStoreManager.getInstanceByUser(getCurrentAccount()).readString(SettingsManager.CAMERA_UPLOAD_BUCKETS_KEY);
        if (TextUtils.isEmpty(confStr)) {
            return CollectionUtils.newArrayList();
        }
        return Arrays.asList(TextUtils.split(confStr, ","));
    }
}
