package com.seafile.seadroid2.view.webview.strategy;

import com.blankj.utilcode.util.AppUtils;
import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;

public class AppVersionGetStrategy implements IWebViewActionStrategy {
    @Override
    public String route(String paramsStr) {
        String v = AppUtils.getAppVersionName();
        int b = AppUtils.getAppVersionCode();
        return v + "-" + b;
    }
}
