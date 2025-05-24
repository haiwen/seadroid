package com.seafile.seadroid2.framework.util;

import com.blankj.utilcode.util.AppUtils;

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
        d("App Version: " + AppUtils.getAppVersionName());
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
