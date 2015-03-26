package com.seafile.seadroid2.util;

import android.util.Log;

/**
 *
 */
public class LogUtils {

    public static void v(String DEBUG_TAG, String message) {
        Log.v(DEBUG_TAG, message);
    }

    public static void d(String DEBUG_TAG, String message) {
        Log.d(DEBUG_TAG, message);
    }

    public static void d(String TAG, String message, Exception e) {
        Log.d(TAG, message, e);
    }

    public static void i(String DEBUG_TAG, String message) {
        Log.i(DEBUG_TAG, message);
    }

    public static void w(String DEBUG_TAG, String message) {
        Log.w(DEBUG_TAG, message);
    }

    public static void e(String DEBUG_TAG, String message) {
        Log.e(DEBUG_TAG, message);
    }

    public static void wtf(String DEBUG_TAG, String message) {
        Log.wtf(DEBUG_TAG, message);
    }
}
