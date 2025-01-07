package com.seafile.seadroid2.view.webview.strategy;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;


public class AppShowToastStrategy implements IWebViewActionStrategy {
    @Override
    public String route(String paramsStr) {
        ToastUtils.showLong(paramsStr);
        return null;
    }
}
