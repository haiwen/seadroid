package com.seafile.seadroid2.view.webview.strategy;

import android.app.Activity;
import android.content.Context;
import android.content.MutableContextWrapper;

import com.seafile.seadroid2.view.webview.IWebViewActionStrategy;

public class PageFinishStrategy implements IWebViewActionStrategy {
    private final Context context;

    public PageFinishStrategy(Context context) {
        this.context = context;
    }

    @Override
    public String route(String paramsStr) {
        if (context != null) {
            MutableContextWrapper c = (MutableContextWrapper) context;
            if (c.getBaseContext() instanceof Activity activity) {
                activity.finish();
            } else {
                throw new IllegalArgumentException("Context is not activity");
            }
        }
        return null;
    }
}
