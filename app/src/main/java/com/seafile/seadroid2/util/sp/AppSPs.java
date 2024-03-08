package com.seafile.seadroid2.util.sp;

import com.seafile.seadroid2.config.Constants;

public class AppSPs {

    /**
     * <p>Whether the app is migrated from v2.x.x to v3.0.0.</p>
     * <p>The migration of the app will last for two versions.</p>
     * <p>If the value is 0, the app is not migrated.</p>
     * <p>If the value is 1, the app is migrated. but the local db file is not deleted </p>
     * <p>If the value is 2, the app is migrated. the local db file is deleted</p>
     */
    public static int isMigratedWhenV300() {
        return SPs.getInt(Constants.App.DATA_IS_MIGRATION, 0);
    }

    public static void setMigratedWhenV300(int v) {
        SPs.put(Constants.App.DATA_IS_MIGRATION, v);
    }

}
