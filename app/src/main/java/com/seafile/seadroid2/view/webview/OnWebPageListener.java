package com.seafile.seadroid2.view.webview;

import android.webkit.WebView;

public interface OnWebPageListener {
    void onPageFinished(WebView view, String url);
}
