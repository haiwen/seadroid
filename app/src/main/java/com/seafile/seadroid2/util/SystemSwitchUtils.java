package com.seafile.seadroid2.util;

import android.content.ContentResolver;
import android.content.Context;
import android.net.ConnectivityManager;

import com.seafile.seadroid2.R;

import java.text.SimpleDateFormat;


public class SystemSwitchUtils {
    private Context context;
    private ConnectivityManager connManager;
    private static SystemSwitchUtils util;

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
    public void syncSwitchUtils() {

        if (!isSyncSwitchOn()) {
            ContentResolver.setMasterSyncAutomatically(!isSyncSwitchOn());
        }

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
}