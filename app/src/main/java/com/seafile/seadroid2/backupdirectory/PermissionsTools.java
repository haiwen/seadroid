package com.seafile.seadroid2.backupdirectory;

import android.os.Build;

public class PermissionsTools {

    public static boolean isAndroid11() {
        return Build.VERSION.SDK_INT >= 30 ? true : false;
    }

}
