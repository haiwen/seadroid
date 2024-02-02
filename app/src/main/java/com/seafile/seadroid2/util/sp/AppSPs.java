package com.seafile.seadroid2.util.sp;

import com.seafile.seadroid2.config.Constants;

public class AppSPs {

    public static boolean isMigratedWhenV300() {
        return SPs.getBoolean(Constants.App.DATA_IS_MIGRATION, false);
    }

    public static void setMigratedWhenV300() {
        SPs.put(Constants.App.DATA_IS_MIGRATION, true);
    }

}
