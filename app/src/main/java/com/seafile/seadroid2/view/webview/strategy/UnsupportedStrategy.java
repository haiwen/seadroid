package com.seafile.seadroid2.view.webview.strategy;


import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;


public class UnsupportedStrategy implements IWebViewActionStrategy {
    @Override
    public String route(String paramsStr) {
        Toasts.show(R.string.not_supported);
        return null;
    }
}
