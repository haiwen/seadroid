package com.seafile.seadroid2.backupdirectory;

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
}
