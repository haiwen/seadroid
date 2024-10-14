package com.seafile.seadroid2.enums;

import android.text.TextUtils;

public enum WebViewPreviewType {
    SDOC, URL;

    public static boolean contains(String value) {
        if (TextUtils.isEmpty(value)){
            return false;
        }

        try {
            WebViewPreviewType.valueOf(value);
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }
}
