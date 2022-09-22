package com.seafile.seadroid2.backupdirectory;

import android.app.Activity;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.UriPermission;
import android.net.Uri;
import android.os.Build;
import android.provider.DocumentsContract;

import com.hjq.permissions.OnPermissionCallback;
import com.hjq.permissions.Permission;
import com.hjq.permissions.XXPermissions;

import java.util.Iterator;
import java.util.List;

public class PermissionsTools {


    public static void getAllNeedPermissions(Activity context, ContentResolver contentResolver) {
        generalPermissionsOfStorage(context);
        specialPermissionsOfStorage(context);
        getAndroidDataPermissionDialog(context, contentResolver);
    }

    /**
     * Get the Android/data directory access permission popover
     *
     * @param activity
     * @param contentResolver
     */
    public static void getAndroidDataPermissionDialog(Activity activity, ContentResolver contentResolver) {
        //Android 11data directory access
        if (Build.VERSION.SDK_INT >= 30) {
            Iterator<UriPermission> it = contentResolver.getPersistedUriPermissions().iterator();
            while (true) {
                if (it.hasNext()) {
                    if (it.next().isReadPermission()) {
                        break;
                    }
                } else {
                    getAndroidDataPermission(activity, contentResolver);//Sandbox storage access permissions
                    break;
                }
            }
        }
    }


    public static void generalPermissionsOfStorage(Context context) {

        //PermissionUtils.permission(PERMISSIONS_STORAGE).request();

        boolean isGet = XXPermissions.isGranted(context, Permission.Group.STORAGE);
        if (isGet) return;

        XXPermissions.with(context)
                //.permission(Permission.MANAGE_EXTERNAL_STORAGE)
                .permission(Permission.Group.STORAGE)
                .unchecked()
                .request(new OnPermissionCallback() {
                    @Override
                    public void onGranted(List<String> permissions, boolean all) {
                        if (all) {
                            if (!isAndroid11()) {
                                //((MainActivity)context).refreshMainShowListView();
                            }
                        }
                    }

                    @Override
                    public void onDenied(List<String> permissions, boolean never) {

                    }
                });
    }

    public static void specialPermissionsOfStorage(Context context) {
//        if (!(Build.VERSION.SDK_INT >= 30 && !Environment.isExternalStorageManager())) {
//
//        } else {
//
//            XXPermissions.with(context)
//                    .permission(Permission.MANAGE_EXTERNAL_STORAGE)
//                    //.permission(Permission.Group.STORAGE)
//                    .unchecked()
//                    .request(new OnPermissionCallback() {
//                        @Override
//                        public void onGranted(List<String> permissions, boolean all) {
//                            if (all) {
//                                if (isAndroid11()) {
//                                }
//                            } else {
//                            }
//                        }
//
//                        @Override
//                        public void onDenied(List<String> permissions, boolean never) {
//                            if (never) {
//                            } else {
//                            }
//                        }
//                    });
//        }
    }

    public static final String URI_ANRROID_DATA = "content://com.android.externalstorage.documents/tree/primary%3AAndroid%2Fdata/document/primary%3AAndroid%2Fdata";//Android/data目录

    public static void getAndroidDataPermission(Activity activity, ContentResolver contentResolver) {
        if (Build.VERSION.SDK_INT >= 30) {
            Iterator<UriPermission> it = contentResolver.getPersistedUriPermissions().iterator();
            while (true) {
                if (it.hasNext()) {
                    if (it.next().isReadPermission()) {
                        break;
                    }
                } else {
                    Uri uri = Uri.parse(URI_ANRROID_DATA);
                    Intent intent1 = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE);
                    intent1.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION
                            | Intent.FLAG_GRANT_WRITE_URI_PERMISSION
                            | Intent.FLAG_GRANT_PERSISTABLE_URI_PERMISSION
                            | Intent.FLAG_GRANT_PREFIX_URI_PERMISSION);
                    intent1.putExtra(DocumentsContract.EXTRA_INITIAL_URI, uri);
                    activity.startActivityForResult(intent1, 11);
                    break;
                }
            }
        }
    }


    public static boolean isAndroid11() {
        return Build.VERSION.SDK_INT >= 30 ? true : false;
    }

}
