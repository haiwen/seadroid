package com.seafile.seadroid2.view.webview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.webkit.CookieManager;
import android.webkit.WebSettings;
import android.webkit.WebView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.util.SeafileLog;
import com.seafile.seadroid2.util.Token2SessionConverts;

public class SeaWebView extends WebView {
    public static final String PATH_ACCOUNT_LOGIN = "accounts/login/";
    public static String URL_LOGIN = null;

    private final SeaWebViewClient mWebViewClient = new SeaWebViewClient();

    public SeaWebView(@NonNull Context context) {
        super(context);
        init();
    }

    public SeaWebView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public SeaWebView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public SeaWebView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init();
    }

    @SuppressLint("SetJavaScriptEnabled")
    private void init() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        URL_LOGIN = account.server + PATH_ACCOUNT_LOGIN;

        WebSettings webSettings = this.getSettings();
        webSettings.setDomStorageEnabled(true);
        webSettings.setDatabaseEnabled(true);
        webSettings.setCacheMode(WebSettings.LOAD_DEFAULT);
//        webSettings.setAppCacheEnabled(true);
//        webSettings.setAppCachePath(getContext().getCacheDir().getAbsolutePath() + "sea_web_cache/");
        webSettings.setLoadWithOverviewMode(true);
        webSettings.setUseWideViewPort(true);
        webSettings.setBlockNetworkImage(false);
        webSettings.setJavaScriptEnabled(true);
        webSettings.setAllowContentAccess(true);
        webSettings.setAllowFileAccess(true);
        webSettings.setAllowFileAccessFromFileURLs(true);
        webSettings.setAllowUniversalAccessFromFileURLs(true);
        webSettings.setJavaScriptCanOpenWindowsAutomatically(false);
        webSettings.setSupportZoom(false);
        webSettings.setBuiltInZoomControls(false);
        webSettings.setDisplayZoomControls(false);
        webSettings.setMixedContentMode(WebSettings.MIXED_CONTENT_COMPATIBILITY_MODE);
        webSettings.setDefaultTextEncodingName("UTF-8");

        CookieManager cookieManager = CookieManager.getInstance();
        cookieManager.setAcceptThirdPartyCookies(this, true);
        cookieManager.setAcceptCookie(true);

        this.setWebViewClient(mWebViewClient);
    }

    public void load(String targetUrl) {
        if (mWebViewClient != null) {
            mWebViewClient.go(targetUrl, this);
        }
    }
}
