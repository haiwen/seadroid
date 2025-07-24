package com.seafile.seadroid2.framework.util;

import com.blankj.utilcode.util.AppUtils;
import com.seafile.seadroid2.BuildConfig;

public class SLogs extends Logs {

    public static void printAppEnvInfo() {
        String brand = android.os.Build.BRAND;
        String model = android.os.Build.MODEL;
        String release = android.os.Build.VERSION.RELEASE;
        String sdk = android.os.Build.VERSION.SDK_INT + "";

        d("App Env Info");
        d("SDK: " + sdk);
        d("Brand: " + brand);
        d("Model: " + model);
        d("Release: " + release);
        d("Build Version: " + BuildConfig.VERSION_NAME);
        d("Build Code:"+ BuildConfig.VERSION_CODE);
    }

    public static String getDeviceBrand() {
        return android.os.Build.BRAND;
    }

    public static String getSystemModel() {
        return android.os.Build.MODEL;
    }

    public static String getSystemVersion() {
        return android.os.Build.VERSION.RELEASE;
    }
}
