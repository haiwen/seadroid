package com.seafile.seadroid2.framework.util;

import android.os.Handler;
import android.os.Looper;

public class SafeLogs {
    private static final Handler mainHandler = new Handler(Looper.getMainLooper());

    public static void e(String e) {
        if (Looper.myLooper() == Looper.getMainLooper()) {
            safeE(e);
        } else {
            mainHandler.post(() -> safeE(e));
        }
    }

    public static void e(String TAG, String e) {
        if (Looper.myLooper() == Looper.getMainLooper()) {
            safeE(TAG + ", " + e);
        } else {
            mainHandler.post(() -> safeE(TAG + ", " + e));
        }
    }

    public static void d(String... logs) {
        if (Looper.myLooper() == Looper.getMainLooper()) {
            safeD(logs);
        } else {
            mainHandler.post(() -> safeD(logs));
        }
    }

    private static void safeD(String... logs) {
        try {
            SLogs.d(logs);
        } catch (Throwable ignored) {
        }
    }

    private static void safeE(String error) {
        try {
            SLogs.e(error);
        } catch (Throwable ignored) {
        }
    }
}
