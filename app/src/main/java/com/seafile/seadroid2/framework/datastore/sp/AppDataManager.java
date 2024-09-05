package com.seafile.seadroid2.framework.datastore.sp;

import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;

public class AppDataManager {
    /**
     * <p>Whether the app is migrated from v2.x.x to v3.0.0.</p>
     */
    public static boolean isMigratedWhenV300() {
        int v = DataStoreManager.getCommonSharePreference().readInteger(DataStoreKeys.DATA_IS_MIGRATION);
        return v > 0;
    }

    /**
     * <p>Whether the app is migrated from v3.0.(0/1/2) to v3.0.3.</p>
     */
    public static boolean isMigratedWhenV303() {
        int v = DataStoreManager.getCommonSharePreference().readInteger(DataStoreKeys.DATA_IS_MIGRATED_WHEN_APP_IS_V303);
        return v > 0;
    }

    public static void setMigratedWhenV300(int v) {
        DataStoreManager.getCommonSharePreference().writeInteger(DataStoreKeys.DATA_IS_MIGRATION, v);
    }

    public static void setMigratedWhenV303(int v) {
        DataStoreManager.getCommonSharePreference().writeInteger(DataStoreKeys.DATA_IS_MIGRATED_WHEN_APP_IS_V303, v);
    }

    public static void savePrivacyPolicyConfirmed(int type) {
        DataStoreManager.getCommonSharePreference().writeInteger(SettingsManager.PRIVACY_POLICY_CONFIRMED, type);
    }

    public static int getPrivacyPolicyConfirmed() {
        return DataStoreManager.getCommonSharePreference().readInteger(SettingsManager.PRIVACY_POLICY_CONFIRMED);
    }


    /**
     * Whether the user has enabled client side encryption
     */
    @Deprecated
    public static boolean isEncryptEnabled() {
        return DataStoreManager.getCommonSharePreference().readBoolean(SettingsManager.CLIENT_ENC_SWITCH_KEY);
    }

    @Deprecated
    public static void writeClientEncSwitch(boolean isChecked) {
        DataStoreManager.getCommonSharePreference().writeBoolean(SettingsManager.CLIENT_ENC_SWITCH_KEY, isChecked);
    }

    public static int readStorageDir() {
        return DataStoreManager.getCommonSharePreference().readInteger(SettingsManager.SHARED_PREF_STORAGE_DIR, Integer.MIN_VALUE);
    }

    public static void writeStorageDir(int nightMode) {
        DataStoreManager.getCommonSharePreference().writeInteger(SettingsManager.SHARED_PREF_STORAGE_DIR, nightMode);
    }
}
