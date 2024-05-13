package com.seafile.seadroid2.framework.util;

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

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.snackbar.Snackbar;
import com.seafile.seadroid2.R;

import java.util.Arrays;
import java.util.List;

public class PermissionUtil {
    public static final int PERMISSIONS_CAMERA = 2;
    public static final int PERMISSIONS_EXTERNAL_STORAGE = 4;
    public static final int PERMISSIONS_POST_NOTIFICATIONS = 8;
    public static final int REQUEST_CODE_MANAGE_ALL_FILES = 16;

    public static boolean checkSelfPermission(Context context, String permission) {
        return ContextCompat.checkSelfPermission(context, permission) == PackageManager.PERMISSION_GRANTED;
    }

    public static boolean shouldShowRequestPermissionRationale(Activity activity, String permission) {
        return ActivityCompat.shouldShowRequestPermissionRationale(activity, permission);
    }

    public static String[] getStoragePermissions() {
//        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
//            return new String[]{Manifest.permission.READ_MEDIA_IMAGES, Manifest.permission.READ_MEDIA_VIDEO};
//        } else
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            return new String[]{Manifest.permission.MANAGE_EXTERNAL_STORAGE};
        } else {
            return new String[]{Manifest.permission.READ_EXTERNAL_STORAGE, Manifest.permission.WRITE_EXTERNAL_STORAGE};
        }
    }

    public static boolean checkExternalStoragePermission(Context context) {
//        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {//13
//            return checkSelfPermission(context, Manifest.permission.READ_MEDIA_IMAGES) ||
//                    checkSelfPermission(context, Manifest.permission.READ_MEDIA_VIDEO);
//        } else
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            return Environment.isExternalStorageManager();
        } else {
            return checkSelfPermission(context, Manifest.permission.WRITE_EXTERNAL_STORAGE);
        }
    }

    public static void requestExternalStoragePermission(
            Context context,
            ActivityResultLauncher<String[]> multiplePermissionLauncher,
            ActivityResultLauncher<Intent> storagePermissionLauncher,
            DialogInterface.OnClickListener onCancel) {

        if (!checkExternalStoragePermission(context)) {
            int msg = R.string.permission_manage_external_storage_rationale;
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R) {
                msg = R.string.permission_read_external_storage_rationale;
            }

            new MaterialAlertDialogBuilder(context)
                    .setMessage(msg)
                    .setNegativeButton(R.string.cancel, (dialog, which) -> {
                        dialog.dismiss();
                        if (onCancel != null) {
                            onCancel.onClick(dialog, which);
                        }
                    })
                    .setPositiveButton(R.string.ok, (dialog, which) -> {
                        dialog.dismiss();

//                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
//                            multiplePermissionLauncher.launch(new String[]{Manifest.permission.READ_MEDIA_IMAGES, Manifest.permission.READ_MEDIA_VIDEO});
//                        } else
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                            storagePermissionLauncher.launch(getManageAllFilesIntent(context));
                        } else {
                            multiplePermissionLauncher.launch(new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE, Manifest.permission.READ_EXTERNAL_STORAGE});
                        }
                    })
                    .show();
        }
    }

    public static void requestExternalStoragePermission(
            Context context,
            ActivityResultLauncher<String[]> multiplePermissionLauncher,
            ActivityResultLauncher<Intent> storagePermissionLauncher) {
        requestExternalStoragePermission(context, multiplePermissionLauncher, storagePermissionLauncher, null);
    }

    public static Intent goToAppSettings(Context context) {
        Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        Uri uri = Uri.fromParts("package", context.getPackageName(), null);
        intent.setData(uri);
        return intent;
    }

    private static boolean shouldShowRequestPermissionsRationale(Activity activity, String[] permissions) {
        for (String permission : permissions) {
            if (shouldShowRequestPermissionRationale(activity, permission)) {
                return true;
            }
        }
        return false;
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
    private static Intent getManageAllFilesIntent(Context context) {
        Intent intent = new Intent();
        intent.setAction(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION);
        Uri uri = Uri.parse("package:" + context.getPackageName());
        intent.setData(uri);
        return intent;
    }
}
