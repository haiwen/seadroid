package com.seafile.seadroid2.framework.util;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.provider.Settings;

import androidx.annotation.RequiresApi;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.CollectionUtils;

import java.util.Arrays;
import java.util.List;

public class PermissionUtil {
    public static final int PERMISSIONS_CAMERA = 2;
    public static final int PERMISSIONS_EXTERNAL_STORAGE = 4;
    public static final int PERMISSIONS_POST_NOTIFICATIONS = 8;
    public static final int REQUEST_CODE_MANAGE_ALL_FILES = 16;

    public static boolean shouldShowRequestPermissionRationale(Activity activity, String permission) {
        return ActivityCompat.shouldShowRequestPermissionRationale(activity, permission);
    }

    private static boolean shouldShowRequestPermissionsRationale(Activity activity, String[] permissions) {
        for (String permission : permissions) {
            if (shouldShowRequestPermissionRationale(activity, permission)) {
                return true;
            }
        }
        return false;
    }

    public static boolean hasNotGrantNotificationPermission(Context context) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            return ContextCompat.checkSelfPermission(context, Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED;
        } else {
            return false; // Android 12 及以下无需手动授权
        }
    }

    public static void requestNotificationPermission(Activity activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            if (hasNotGrantNotificationPermission(activity)) {
                ActivityCompat.requestPermissions(activity,
                        new String[]{Manifest.permission.POST_NOTIFICATIONS},
                        PERMISSIONS_POST_NOTIFICATIONS);
            }
        }
    }

    public static boolean hasStoragePermission(Context context) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            // Android 11 and +
            return Environment.isExternalStorageManager(); // need MANAGE_EXTERNAL_STORAGE
        } else {
            // Android 10 and -
            return ContextCompat.checkSelfPermission(context, Manifest.permission.READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED;
        }
    }


    @RequiresApi(Build.VERSION_CODES.R)
    private static boolean canRequestAllFilesPermission(Context context) {
        return manifestHasAllFilesPermission(context) && hasManageAllFilesActivity(context);
    }

    @RequiresApi(Build.VERSION_CODES.R)
    private static boolean hasManageAllFilesActivity(Context context) {
        Intent intent = getManageAllFilesIntent(context);

        List<ResolveInfo> launchables = context.getPackageManager().queryIntentActivities(intent, PackageManager.GET_RESOLVED_FILTER);
        return !CollectionUtils.isEmpty(launchables);
    }

    @RequiresApi(Build.VERSION_CODES.R)
    private static boolean manifestHasAllFilesPermission(Context context) {
        try {
            PackageInfo packageInfo = context.getPackageManager().getPackageInfo(context.getPackageName(), PackageManager.GET_PERMISSIONS);
            if (packageInfo == null) {
                return false;
            }
            if (packageInfo.requestedPermissions == null) {
                return false;
            }

            List<String> ps = Arrays.asList(packageInfo.requestedPermissions);

            if (CollectionUtils.isEmpty(ps)) {
                return false;
            }

            return ps.contains(Manifest.permission.MANAGE_EXTERNAL_STORAGE);
        } catch (PackageManager.NameNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    @RequiresApi(api = Build.VERSION_CODES.R)
    public static Intent getManageAllFilesIntent(Context context) {
        Intent intent = new Intent();
        intent.setAction(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION);
        Uri uri = Uri.parse("package:" + context.getPackageName());
        intent.setData(uri);
        return intent;
    }
}
