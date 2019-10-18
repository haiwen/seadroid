package com.seafile.seadroid2.util;

import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.Uri;
import android.os.Build;
import android.provider.Settings;
import android.text.TextUtils;

import com.seafile.seadroid2.R;

import java.text.SimpleDateFormat;


public class SystemSwitchUtils {
    private Context context;
    private ConnectivityManager connManager;
    private static SystemSwitchUtils util;

    public final static int HUAWEI_PHONEMODEL = 1;

    public final static int XIAOMI_PHONEMODEL = 2;

    public final static int VIVO_PHONEMODEL = 3;

    public final static int OPPO_PHONEMODEL = 4;

    public final static int SAMSUNG_PHONEMODEL = 5;

    public final static int DEFAULT_PHONEMODEL = 0;

    public static SystemSwitchUtils getInstance(Context context){
        if (util == null) {
            util = new SystemSwitchUtils(context);
        }
        return util;

    }

    private SystemSwitchUtils(Context context) {
        super();
        this.context = context;
    }


    /**
     * Open Sync
     * @return
     */
    @SuppressWarnings("deprecation")
    public boolean isSyncSwitchOn() {
        if (connManager == null) {
            connManager = (ConnectivityManager) context
                    .getSystemService(Context.CONNECTIVITY_SERVICE);
        }

        return connManager.getBackgroundDataSetting()
                && ContentResolver.getMasterSyncAutomatically();
    }

    /**
     * synchro switch
     */
    public void syncSwitchUtils(Context ct) {

        if (!isSyncSwitchOn()) {
            ContentResolver.setMasterSyncAutomatically(!isSyncSwitchOn());
        }
        onViewClicked(getDeviceType(), ct);

    }

    public static String date2TimeStamp(String date_str, String format) {
        try {
            SimpleDateFormat sdf = new SimpleDateFormat(format);
            return String.valueOf(sdf.parse(date_str).getTime() / 1000);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "";
    }

    public static String parseDateTime(String dateString) {
        if (dateString == null) return null;
        if (dateString.contains("T")) dateString = dateString.replace('T', ' ');
        String[] arr1 = dateString.split("\\+");
        return Utils.translateCommitTime(Long.parseLong(date2TimeStamp(arr1[0], "yyyy-MM-dd HH:mm:ss")) * 1000);
    }

    public static String obj_type(Context ct, String obj_type, String op_type) {
        if (obj_type.equals("repo")) {
            if (op_type.equals("create")) {
                return ct.getString(R.string.create_new_repo);
            } else if (op_type.equals("rename")) {
                return ct.getString(R.string.rename_repo);
            } else if (op_type.equals("delete")) {
                return ct.getString(R.string.delete_repo_title);
            } else if (op_type.equals("restore")) {
                return ct.getString(R.string.recover_library);
            } else if (op_type.equals("edit")) {
                return ct.getString(R.string.edit);
            } else {
                return "";
            }
        } else if (obj_type.equals("dir")) {
            if (op_type.equals("create")) {
                return ct.getString(R.string.create_new_dir);
            } else if (op_type.equals("rename")) {
                return ct.getString(R.string.rename_dir);
            } else if (op_type.equals("delete")) {
                return ct.getString(R.string.delete_dir);
            } else if (op_type.equals("restore")) {
                return ct.getString(R.string.recover_folder);
            } else if (op_type.equals("move")) {
                return ct.getString(R.string.move_folder);
            } else if (op_type.equals("edit")) {
                return ct.getString(R.string.edit);
            } else {
                return "";
            }
        } else if (obj_type.equals("file")) {
            if (op_type.equals("create")) {
                return ct.getString(R.string.create_new_file);
            } else if (op_type.equals("rename")) {
                return ct.getString(R.string.rename_file);
            } else if (op_type.equals("delete")) {
                return ct.getString(R.string.delete_file_f);
            } else if (op_type.equals("restore")) {
                return ct.getString(R.string.recover_file);
            } else if (op_type.equals("move")) {
                return ct.getString(R.string.move_file);
            } else if (op_type.equals("update")) {
                return ct.getString(R.string.update_file);
            } else if (op_type.equals("edit")) {
                return ct.getString(R.string.edit);
            } else {
                return "";
            }
        } else if (obj_type.equals("draft")) {
            if (op_type.equals("create")) {
                return ct.getString(R.string.create_draft);
            } else if (op_type.equals("rename")) {
                return ct.getString(R.string.rename_draft);
            } else if (op_type.equals("delete")) {
                return ct.getString(R.string.del_draft);
            } else if (op_type.equals("update")) {
                return ct.getString(R.string.update_draft);
            } else if (op_type.equals("publish")) {
                return ct.getString(R.string.release_draft);
            } else if (op_type.equals("edit")) {
                return ct.getString(R.string.edit);
            } else {
                return "";
            }
        } else if (obj_type.equals("files")) {
            if (op_type.equals("create")) {
                return ct.getString(R.string.create_files);
            } else {
                return "";
            }
        } else {
            return "";
        }
    }

    public static String getDeviceBrand() {
        return android.os.Build.BRAND;
    }

    public static int getDeviceType() {

        int phoneModel;

        String deviceBrand = getDeviceBrand();

        System.out.println(" deviceBrand : " + deviceBrand);

        if (!TextUtils.isEmpty(deviceBrand)) {

            if ("honor".equals(getDeviceBrand().toLowerCase()) || "huawei".equals(getDeviceBrand().toLowerCase())) {

                phoneModel = 1;

            } else if ("xiaomi".equals(getDeviceBrand().toLowerCase())) {

                phoneModel = 2;

            } else if ("vivo".equals(getDeviceBrand().toLowerCase())) {

                phoneModel = 3;

            } else if ("oppo".equals(getDeviceBrand().toLowerCase())) {

                phoneModel = 4;

            } else if ("samsung".equals(getDeviceBrand().toLowerCase())) {

                phoneModel = 5;

            } else {

                phoneModel = 0;

            }

        } else {

            phoneModel = 0;

        }

        return phoneModel;

    }

    public static void onViewClicked(int phoneModel, Context context) {

        Intent intent = new Intent();

        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

        ComponentName comp = null;

        switch (phoneModel) {

            case HUAWEI_PHONEMODEL:

                //huawei 9.0 ACTIVITY com.huawei.systemmanager.startupmgr.ui.StartupNormalAppListActivity

                // 8.0 com.huawei.systemmanager/.appcontrol.activity.StartupAppControlActivity

                // 7.0 6.0 com.huawei.systemmanager/.startupmgr.ui.StartupNormalAppListActivity

                //5.0authority management com.huawei.systemmanager/com.huawei.permissionmanager.ui.MainActivity

                //5.0 Boot up self-turn-on com.huawei.systemmanager/.optimize.bootstart.BootStartActivity

                if (Build.VERSION.SDK_INT >= 28) {

                    comp = new ComponentName("com.huawei.systemmanager", "com.huawei.systemmanager.startupmgr.ui.StartupNormalAppListActivity");

                } else if (Build.VERSION.SDK_INT >= 26) {

                    comp = new ComponentName("com.huawei.systemmanager",

                            "com.huawei.systemmanager.appcontrol.activity.StartupAppControlActivity");

                } else if (Build.VERSION.SDK_INT >= 23) {

                    comp = new ComponentName("com.huawei.systemmanager",

                            "com.huawei.systemmanager.startupmgr.ui.StartupNormalAppListActivity");

                } else {

                    comp = new ComponentName("com.huawei.systemmanager",

                            "com.huawei.systemmanager.com.huawei.permissionmanager.ui.MainActivity");

                }

                break;

            case XIAOMI_PHONEMODEL:

                // 8.0 7.0 6.0 com.miui.securitycenter/com.miui.permcenter.autostart.AutoStartManagementActivity

                comp = new ComponentName("com.miui.securitycenter",

                        "com.miui.permcenter.autostart.AutoStartManagementActivity");

                break;

            case VIVO_PHONEMODEL:

                //8.1.0 7.0 6.0 com.vivo.permissionmanager/.activity.BgStartUpManagerActivity

                //5.0 4.0com.iqoo.secure/.ui.phoneoptimize.SoftwareManagerActivity

                //7.0 6.0 com.vivo.permissionmanager/.activity.PurviewTabActivity

                if (Build.VERSION.SDK_INT >= 23) {

                    comp = new ComponentName("com.vivo.permissionmanager",

                            "com.vivo.permissionmanager.activity.PurviewTabActivity");

                } else {

                    comp = new ComponentName("com.iqoo.secure",

                            "com.iqoo.secure.ui.phoneoptimize.SoftwareManagerActivity");

                }

                break;

            case OPPO_PHONEMODEL:

                //Permission setting interface8.0 7.0 com.coloros.safecenter/com.coloros.privacypermissionsentry.PermissionTopActivity

                //  com.color.safecenter/.permission.PermissionTopActivity

                //Self-start management interface 8.1.0 com.coloros.safecenter/.startupapp.StartupAppListActivity

                //Self-start management interface 7.0 6.0 com.coloros.safecenter/.startupapp.StartupAppListActivity

                //5.0 com.oppo.safe/.permission.startup.StartupAppListActivity

                //4.4.4 R series com.color.safecenter/.permission.startup.StartupAppListActivity

                //Associated start management interface 8.0 7.0 无6.0 com.coloros.safecenter/.startupapp.AssociateStartActivity

                if (Build.VERSION.SDK_INT >= 23) {

                    comp = new ComponentName("com.coloros.safecenter",

                            "com.coloros.safecenter.startupapp.StartupAppListActivity");

                } else {

                    comp = new ComponentName("com.color.safecenter",

                            "com.color.safecenter.permission.startup.StartupAppListActivity");

                }

                break;

            case SAMSUNG_PHONEMODEL:

                //8.0  7.1.1  com.samsung.android.sm_cn/com.samsung.android.sm.ui.ram.AutoRunActivity

                comp = new ComponentName("com.samsung.android.sm_cn",

                        "com.samsung.android.sm_cn.com.samsung.android.sm.ui.ram.AutoRunActivity");

                break;

            case DEFAULT_PHONEMODEL:

                comp = null;

                break;

        }
        try {

            if (comp == null) {

                intent.setAction(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);

                Uri uri = Uri.fromParts("package", context.getPackageName(), null);

                intent.setData(uri);

                context.startActivity(intent);

            } else {

                intent.setComponent(comp);

                context.startActivity(intent);

            }

        } catch (Exception e) {


            Intent intentSetting = new Intent();

            intentSetting.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

            intentSetting.setAction(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);

            Uri uri = Uri.fromParts("package", context.getPackageName(), null);

            intentSetting.setData(uri);

            context.startActivity(intentSetting);

        }

    }
}