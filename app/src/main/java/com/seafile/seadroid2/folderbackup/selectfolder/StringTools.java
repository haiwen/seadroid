package com.seafile.seadroid2.folderbackup.selectfolder;

import android.text.TextUtils;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;

public class StringTools {

    public static boolean isEmpty(String s) {
        return s == null || s.trim().isEmpty() ? true : false;
    }

    public static Long getOnlyNumber(String s) {
        String temp = s.replaceAll("[^0-9]", "");
        if (temp.equals("")) {
            return -1L;
        }
        Long number = Long.valueOf(temp);
        if (number == null) {
            return -1L;
        }
        return Long.valueOf(temp);
    }

    public static <T> List<T> getJsonToList(String strJson) {
        List<T> list = new ArrayList<T>();
        if (TextUtils.isEmpty(strJson)) {
            return list;
        }
        Gson gson = new Gson();
        list = gson.fromJson(strJson, new TypeToken<List<T>>() {
        }.getType());
        return list;
    }

    public static boolean checkFolderUploadNetworkAvailable() {
        if (!Utils.isNetworkOn()) {
            return false;
        }

        // user does not allow mobile connections
        if (!Utils.isWiFiOn() && !SettingsManager.instance().isFolderBackupDataPlanAllowed()) {
            return false;
        }
        
        // Wi-Fi or 2G/3G/4G connections available
        return true;
    }

}
