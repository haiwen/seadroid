package com.seafile.seadroid2.util;

import android.util.Log;

import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Log Utils
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

    public static void w(String DEBUG_TAG, FileNotFoundException e) {
        Log.w(DEBUG_TAG, e);
    }

    public static void w(String DEBUG_TAG, IOException e) {
        Log.w(DEBUG_TAG, e);
    }

    public static void w(String DEBUG_TAG, String message, Exception e) {
        Log.w(DEBUG_TAG, message, e);
    }

    public static void e(String DEBUG_TAG, String message) {
        Log.e(DEBUG_TAG, message);
    }

    public static void e(String DEBUG_TAG, String message, Throwable throwable) {
        Log.e(DEBUG_TAG, message, throwable);
    }

    public static void e(String DEBUG_TAG, String message, Exception e) {
        Log.e(DEBUG_TAG, message, e);
    }

    public static void e(String DEBUG_TAG, String message, OutOfMemoryError e) {
        Log.e(DEBUG_TAG, message, e);
    }

    public static void wtf(String DEBUG_TAG, String message) {
        Log.wtf(DEBUG_TAG, message);
    }
}
