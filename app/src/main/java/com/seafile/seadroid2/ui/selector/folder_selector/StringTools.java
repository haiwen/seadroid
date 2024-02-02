package com.seafile.seadroid2.ui.selector.folder_selector;

import android.text.TextUtils;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.util.ArrayList;
import java.util.List;

public class StringTools {

    public static Long getOnlyNumber(String s) {
        String temp = s.replaceAll("[^0-9]", "");
        if (temp.equals("")) {
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
}
