package com.seafile.seadroid2.view.webview.strategy;

import com.blankj.utilcode.util.BarUtils;
import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;

public class PageStatusHeightGetStrategy implements IWebViewActionStrategy {
    @Override
    public String route(String paramsStr) {
        //page.status.height.get
        return String.valueOf(BarUtils.getStatusBarHeight());
    }
}
