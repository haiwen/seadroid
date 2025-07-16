package com.seafile.seadroid2;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;

/**
 * This receiver is called whenever the system has booted or
 * the Seadroid app has been upgraded to a new version.
 * It can be used to start up background services.
 */
@Deprecated
public class BootAutostart extends BroadcastReceiver {
    private static final String DEBUG_TAG = "BootAutostart";


    /**
     * On android 15, restrictions on BOOT_COMPLETED broadcast receivers that start the foreground service:
     * <br>
     * <a href="https://developer.android.com/about/versions/15/behavior-changes-15?hl=zh-cn#fgs-boot-completed">fgs-boot-completed</a>
     * <br>
     * <p>
     * This method will be excecuted after
     * - booting the device
     * - upgrade of the Seadroid package
     * </p>
     */
    public void onReceive(Context context, Intent intent) {
        if (intent == null || intent.getAction() == null) {
            return;
        }


//        if (TextUtils.equals(Intent.ACTION_BOOT_COMPLETED, intent.getAction())
//                || TextUtils.equals(Intent.ACTION_MY_PACKAGE_REPLACED, intent.getAction())) {
//        }
    }


}
