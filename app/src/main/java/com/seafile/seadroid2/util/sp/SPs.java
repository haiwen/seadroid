package com.seafile.seadroid2.util.sp;

import android.content.Context;
import android.content.SharedPreferences;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.SPUtils;
import com.blankj.utilcode.util.Utils;
import com.seafile.seadroid2.config.Constants;

public class SPs {
    private static final String SP_NAME = "seadroid_sp";

    public static boolean containsKey(String key) {
        return SPUtils.getInstance(SP_NAME).contains(key);
    }

    ////////////////////////put////////////////
    public static void put(@NonNull final String key, final String value) {
        SPUtils.getInstance(SP_NAME).put(key, value);
    }

    public static void put(@NonNull final String key, final int value) {
        SPUtils.getInstance(SP_NAME).put(key, value);
    }

    public static void put(@NonNull final String key, final boolean value) {
        SPUtils.getInstance(SP_NAME).put(key, value);
    }

    ////////////////////////get////////////////
    public static String getString(@NonNull final String key) {
        return SPUtils.getInstance(SP_NAME).getString(key);
    }

    public static String getString(@NonNull final String key, String defaultValue) {
        return SPUtils.getInstance(SP_NAME).getString(key, defaultValue);
    }

    public static int getInt(@NonNull final String key) {
        return SPUtils.getInstance(SP_NAME).getInt(key);
    }

    public static int getInt(@NonNull final String key, final int defaultValue) {
        return SPUtils.getInstance(SP_NAME).getInt(key, defaultValue);
    }

    public static boolean getBoolean(@NonNull final String key) {
        return SPUtils.getInstance(SP_NAME).getBoolean(key);
    }

    public static boolean getBoolean(@NonNull final String key, final boolean defaultValue) {
        return SPUtils.getInstance(SP_NAME).getBoolean(key, defaultValue);
    }

    public static void registerOnSharedPreferenceChangeListener(SharedPreferences.OnSharedPreferenceChangeListener listener) {
        SharedPreferences sp = Utils.getApp().getSharedPreferences(SP_NAME, Context.MODE_PRIVATE);
        sp.registerOnSharedPreferenceChangeListener(listener);
    }

    public static void unregisterOnSharedPreferenceChangeListener(SharedPreferences.OnSharedPreferenceChangeListener listener) {
        SharedPreferences sp = Utils.getApp().getSharedPreferences(SP_NAME, Context.MODE_PRIVATE);
        sp.unregisterOnSharedPreferenceChangeListener(listener);
    }
}
