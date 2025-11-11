package com.seafile.seadroid2.view.webview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.webkit.CookieManager;
import android.webkit.WebSettings;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.GsonUtils;
import com.github.lzyzsd.jsbridge.BridgeHandler;
import com.github.lzyzsd.jsbridge.CallBackFunction;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.WebViewActionConstant;
import com.seafile.seadroid2.framework.model.WebRouteModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.view.NestedWebView;
import com.seafile.seadroid2.view.webview.strategy.AppShowToastStrategy;
import com.seafile.seadroid2.view.webview.strategy.AppVersionGetStrategy;
import com.seafile.seadroid2.view.webview.strategy.EmptyStrategy;
import com.seafile.seadroid2.view.webview.strategy.PageFinishStrategy;
import com.seafile.seadroid2.view.webview.strategy.PageStatusHeightGetStrategy;

import java.util.Locale;

public class SeaWebView extends NestedWebView {
    public static final String PATH_ACCOUNT_LOGIN = "accounts/login/";
    public static String URL_LOGIN = null;
    private final String JS_FUNCTION_NAME = "callJsFunction";
    private final String ANDROID_FUNCTION_NAME = "callAndroidFunction";

    private final SeaWebViewClient mWebViewClient = new SeaWebViewClient(this);

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

//    public SeaWebView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
//        super(context, attrs, defStyleAttr, defStyleRes);
//        init();
//    }

    @SuppressLint("SetJavaScriptEnabled")
    private void init() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return;
        }

        URL_LOGIN = account.server + PATH_ACCOUNT_LOGIN;

        WebSettings webSettings = this.getSettings();
        webSettings.setDomStorageEnabled(true);
        webSettings.setDatabaseEnabled(true);
        webSettings.setCacheMode(WebSettings.LOAD_DEFAULT);
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
        webSettings.setDatabaseEnabled(true);
        webSettings.setDisplayZoomControls(false);
        webSettings.setMixedContentMode(WebSettings.MIXED_CONTENT_COMPATIBILITY_MODE);
        webSettings.setDefaultTextEncodingName("UTF-8");

        //ua
        String ua = webSettings.getUserAgentString();
        if (TextUtils.isEmpty(ua)) {
            ua = Constants.UA.SEAFILE_ANDROID_UA;
        } else {
            ua += " " + Constants.UA.SEAFILE_ANDROID_UA;
        }
        webSettings.setUserAgentString(ua);
        SLogs.d("seafile webview ua: " + ua);

        CookieManager cookieManager = CookieManager.getInstance();
        cookieManager.setAcceptThirdPartyCookies(this, true);
        cookieManager.setAcceptCookie(true);

        this.setWebViewClient(mWebViewClient);

        registerCommonHandler();
    }

    public void setOnWebPageListener(OnWebPageListener onWebPageListener) {
        mWebViewClient.setOnWebPageListener(onWebPageListener);
        this.setWebViewClient(mWebViewClient);
    }

    public void load(String targetUrl) {
        mWebViewClient.go(targetUrl, this);
    }

    public void loadDirectly(String targetUrl) {
        mWebViewClient.loadWithoutToken(targetUrl, this);
    }


    private void registerCommonHandler() {
        //
        this.registerHandler(ANDROID_FUNCTION_NAME, new BridgeHandler() {
            @Override
            public void handler(String data, CallBackFunction function) {
                callAndroidFunction(data, function);
            }
        });
    }

    public OnWebDataCallback onWebDataCallback;

    public void setOnWebDataCallback(OnWebDataCallback onWebDataCallback) {
        this.onWebDataCallback = onWebDataCallback;
    }

    private void callAndroidFunction(String data, CallBackFunction function) {
        if (TextUtils.isEmpty(data)
                || data.toLowerCase(Locale.getDefault()).equals("null")
                || data.toLowerCase(Locale.getDefault()).equals("undefined")) {
            return;
        }

        try {
            WebRouteModel model = GsonUtils.fromJson(data, WebRouteModel.class);
            if (TextUtils.equals(model.action, WebViewActionConstant.SDOC_EDITOR_CONTENT_SELECT)) {
                this.onWebDataCallback.onCallback(model);
                return;
            }

            if (TextUtils.equals(model.action, WebViewActionConstant.SDOC_EDITOR_OPERATION_EXECUTE)) {
                this.onWebDataCallback.onCallback(model);
                return;
            }

            String result = getResult(model);
            if (!TextUtils.isEmpty(result)) {
                function.onCallBack(result);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String getResult(WebRouteModel model) {
        IWebViewActionStrategy strategy;
        switch (model.action) {
            case WebViewActionConstant.PAGE_FINISH: {
                strategy = new PageFinishStrategy(getContext());
            }
            break;
            case WebViewActionConstant.APP_VERSION_GET: {
                strategy = new AppVersionGetStrategy();
            }
            break;
            case WebViewActionConstant.APP_TOAST_SHOW: {
                strategy = new AppShowToastStrategy();
            }
            break;
            case WebViewActionConstant.PAGE_STATUS_HEIGHT_GET: {
                strategy = new PageStatusHeightGetStrategy();
            }
            break;
            default:
                strategy = new EmptyStrategy();
                break;
        }
        return strategy.route(model.data);
    }

    public void callJsFunction(String action, String data) {
        callJsFunction(action, data, null);
    }

    public void callJsFunction(String action, String data, CallBackFunction callback) {
        WebRouteModel model = new WebRouteModel();
        model.action = action;
        model.data = data;
        model.v = 2;
        String g = GsonUtils.toJson(model);
        SLogs.d(JS_FUNCTION_NAME, " param => ", g);
        callHandler(JS_FUNCTION_NAME, g, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(JS_FUNCTION_NAME, " callback data => ", data);
                if (callback != null) {
                    callback.onCallBack(data);
                }
            }
        });
    }
}
