package com.seafile.seadroid2.framework.util;

import android.app.ActivityManager;
import android.content.Context;
import android.net.ConnectivityManager;

/** Checks system restrictions before starting automatic backup work. */
public final class BackgroundExecutionPolicy {
    private BackgroundExecutionPolicy() {}

    public static boolean shouldDefer(Context context) {
        ActivityManager.RunningAppProcessInfo process = new ActivityManager.RunningAppProcessInfo();
        ActivityManager.getMyMemoryState(process);
        if (process.importance <= ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND) {
            return false;
        }

        ActivityManager activities = context.getSystemService(ActivityManager.class);
        if (activities != null && activities.isBackgroundRestricted()) {
            return true;
        }

        ConnectivityManager network = context.getSystemService(ConnectivityManager.class);
        return network != null && network.isActiveNetworkMetered()
                && network.getRestrictBackgroundStatus()
                == ConnectivityManager.RESTRICT_BACKGROUND_STATUS_ENABLED;
    }
}
