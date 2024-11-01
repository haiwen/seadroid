package com.seafile.seadroid2.view.webview;

import android.app.Activity;
import android.content.Context;
import android.content.MutableContextWrapper;
import android.os.Looper;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;

import java.util.Stack;

public class PreloadWebView {
    private static final int CACHED_WEB_VIEW_MAX_NUM = 2;
    private static final Stack<SeaWebView> mCachedWebViewStack = new Stack<>();

    private PreloadWebView() {

    }

    public static PreloadWebView getInstance() {
        return Holder.INSTANCE;
    }

    private static class Holder {
        private static final PreloadWebView INSTANCE = new PreloadWebView();
    }

    public void preload() {
        Looper.myQueue().addIdleHandler(() -> {
            if (mCachedWebViewStack.size() < CACHED_WEB_VIEW_MAX_NUM) {
                mCachedWebViewStack.push(buildWebView());
            }
            return false;
        });
    }

    private SeaWebView buildWebView() {
        SeaWebView webView = new SeaWebView(new MutableContextWrapper(SeadroidApplication.getAppContext()));
        webView.setId(R.id.webview);
        return webView;
    }

    /**
     * context must be a Class extends Activity
     */
    public SeaWebView getWebView(Context context) {
        boolean isActivity = context instanceof Activity;
        if (!isActivity) {
            throw new IllegalArgumentException("context must be a Class extends Activity");
        }

        if (mCachedWebViewStack.isEmpty()) {
            SeaWebView webView = buildWebView();
            MutableContextWrapper contextWrapper = (MutableContextWrapper) webView.getContext();
            contextWrapper.setBaseContext(context);
            return webView;
        }

        SeaWebView webView = mCachedWebViewStack.pop();
        MutableContextWrapper contextWrapper = (MutableContextWrapper) webView.getContext();
        contextWrapper.setBaseContext(context);
        return webView;
    }
}
