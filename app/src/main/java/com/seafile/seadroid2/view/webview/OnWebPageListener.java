package com.seafile.seadroid2.view.webview;

import android.graphics.Bitmap;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebView;

public interface OnWebPageListener {

    void onPageStarted(WebView view, String url, Bitmap favicon);

    void onPageFinished(WebView view, String url);
    void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error);
    void onReceivedHttpError(WebView view, WebResourceRequest request, WebResourceResponse errorResponse);
}
