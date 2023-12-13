package com.seafile.seadroid2.view.webview;

import android.os.Build;
import android.text.TextUtils;
import android.webkit.CookieManager;

import java.util.ArrayList;
import java.util.List;

import okhttp3.Cookie;
import okhttp3.HttpUrl;

public class SupportCookieManager {
    public static String getCookie(String url) {
        CookieManager manager = CookieManager.getInstance();
        if (manager == null) {
            return null;
        }

        return manager.getCookie(url);
    }

    public static List<Cookie> getCookieForOkHttp(HttpUrl url) {
        String c = getCookie(url.toString());
        if (TextUtils.isEmpty(c)) {
            return null;
        }
        Cookie cookie = Cookie.parse(url, c);
        ArrayList<Cookie> cookies = new ArrayList<>();
        cookies.add(cookie);
        return cookies;
    }
}
