package com.seafile.seadroid2.framework.datastore.sp;

import androidx.appcompat.app.AppCompatDelegate;

import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;

public class AppDataManager {
    /**
     * <p>Whether the app is migrated from v2.x.x to v3.0.0.</p>
     * <p>The migration of the app will last for two versions.</p>
     * <p>If the value is 0, the app is not migrated.</p>
     * <p>If the value is 1, the app is migrated. but the local db file is not deleted</p>
     */
    public static boolean isMigratedWhenV300() {
        int v = DataStoreManager.getCommonInstance().readInteger(DataStoreKeys.DATA_IS_MIGRATION);
        return v > 0;
    }

    public static void setMigratedWhenV300(int v) {
        DataStoreManager.getCommonInstance().writeInteger(DataStoreKeys.DATA_IS_MIGRATION, v);
    }

    public static void savePrivacyPolicyConfirmed(int type) {
        DataStoreManager.getCommonInstance().writeInteger(SettingsManager.PRIVACY_POLICY_CONFIRMED, type);
    }

    public static int getPrivacyPolicyConfirmed() {
        return DataStoreManager.getCommonInstance().readInteger(SettingsManager.PRIVACY_POLICY_CONFIRMED);
    }


    /**
     * Whether the user has enabled client side encryption
     */
    public static boolean isEncryptEnabled() {
        return DataStoreManager.getCommonInstance().readBoolean(SettingsManager.CLIENT_ENC_SWITCH_KEY);
    }

    public static void writeClientEncSwitch(boolean isChecked) {
        DataStoreManager.getCommonInstance().writeBoolean(SettingsManager.CLIENT_ENC_SWITCH_KEY, isChecked);
    }

    public static int readStorageDir() {
        return DataStoreManager.getCommonInstance().readInteger(SettingsManager.SHARED_PREF_STORAGE_DIR, Integer.MIN_VALUE);
    }

    public static void writeStorageDir(int nightMode) {
        DataStoreManager.getCommonInstance().writeInteger(SettingsManager.SHARED_PREF_STORAGE_DIR, nightMode);
    }

    public static void writeDarkMode(int nightMode) {
        DataStoreManager.getCommonInstance().writeInteger(DataStoreKeys.KEY_DARK_MODE, nightMode);
    }

    public static int readDarkMode() {
        return DataStoreManager.getCommonInstance().readInteger(DataStoreKeys.KEY_DARK_MODE, AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
    }

}
