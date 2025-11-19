package com.seafile.seadroid2.view.webview.strategy;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;

public class EmptyStrategy implements IWebViewActionStrategy {
    @Override
    public String route(String paramsStr) {
        ToastUtils.showLong(R.string.not_supported);
        return null;
    }
}
