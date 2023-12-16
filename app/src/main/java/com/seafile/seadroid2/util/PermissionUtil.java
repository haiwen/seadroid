package com.seafile.seadroid2.util;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.provider.Settings;
import android.view.View;

import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.snackbar.Snackbar;
import com.seafile.seadroid2.R;

import java.util.Arrays;
import java.util.List;

public class PermissionUtil {
    public final static int PERMISSIONS_CAMERA = 2;
    public final static int PERMISSIONS_EXTERNAL_STORAGE = 4;
    public final static int PERMISSIONS_POST_NOTIFICATIONS = 8;
    public final static int REQUEST_CODE_MANAGE_ALL_FILES = 16;

    public static boolean checkSelfPermission(Context context, String permission) {
        return ContextCompat.checkSelfPermission(context, permission) == PackageManager.PERMISSION_GRANTED;
    }

    public static boolean shouldShowRequestPermissionRationale(Activity activity, String permission) {
        return ActivityCompat.shouldShowRequestPermissionRationale(activity, permission);
    }

    public static boolean checkExternalStoragePermission(Context context) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            return Environment.isExternalStorageManager() ||
                    (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU ?
                            checkSelfPermission(context, Manifest.permission.READ_MEDIA_IMAGES) || checkSelfPermission(context, Manifest.permission.READ_MEDIA_VIDEO) :
                            checkSelfPermission(context, Manifest.permission.READ_EXTERNAL_STORAGE));
        } else {
            return checkSelfPermission(context, Manifest.permission.WRITE_EXTERNAL_STORAGE);
        }
    }

    public static void requestNotificationPermission(Activity activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            if (!checkSelfPermission(activity, Manifest.permission.POST_NOTIFICATIONS)) {
                ActivityCompat.requestPermissions(
                        activity,
                        new String[]{Manifest.permission.POST_NOTIFICATIONS},
                        PERMISSIONS_POST_NOTIFICATIONS
                );
            }
        }
    }

    public static void requestExternalStoragePermission(AppCompatActivity activity) {
        if (!checkExternalStoragePermission(activity)) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R && canRequestAllFilesPermission(activity)) {
                new AlertDialog.Builder(activity)
                        .setMessage(R.string.permission_manage_exteral_storage_rationale)
                        .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialog, int which) {
                                dialog.dismiss();
                            }
                        })
                        .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialog, int which) {
                                dialog.dismiss();
                                Intent intent = getManageAllFilesIntent(activity);
                                activity.startActivity(intent);
                            }
                        })
                        .show();
            } else {
                requestStoragePermission(activity);
            }
        }
    }

    private static void requestStoragePermission(Activity activity) {
        String[] ps = getStoragePermissions();
        if (shouldShowRequestPermissionsRationale(activity, ps)) {
            Snackbar.make(activity.findViewById(android.R.id.content),
                            R.string.permission_read_exteral_storage_rationale,
                            Snackbar.LENGTH_INDEFINITE)
                    .setAction(R.string.settings, new View.OnClickListener() {
                        @Override
                        public void onClick(View view) {
                            ActivityCompat.requestPermissions(activity,
                                    new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE},
                                    PERMISSIONS_EXTERNAL_STORAGE);
                        }
                    })
                    .show();

        } else {
            ActivityCompat.requestPermissions(activity, ps, PERMISSIONS_EXTERNAL_STORAGE);
        }
    }

    private static boolean shouldShowRequestPermissionsRationale(Activity activity, String[] permissions) {
        for (String permission : permissions) {
            if (shouldShowRequestPermissionRationale(activity, permission)) {
                return true;
            }
        }
        return false;
    }

    private static String[] getStoragePermissions() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                // use granular media permissions
                return new String[]{Manifest.permission.READ_MEDIA_IMAGES, Manifest.permission.READ_MEDIA_VIDEO};
            } else {
                return new String[]{Manifest.permission.READ_EXTERNAL_STORAGE};
            }
        } else {
            return new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE};
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

    @RequiresApi(Build.VERSION_CODES.R)
    private static Intent getManageAllFilesIntent(Context context) {
        Intent intent = new Intent();
        intent.setAction(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION);
        Uri uri = Uri.parse("package:" + context.getPackageName());
        intent.setData(uri);
        return intent;
    }
}
