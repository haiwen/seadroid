package com.seafile.seadroid2.ui.activity;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.provider.Settings;
import android.util.Log;
import android.view.View;
import android.view.animation.AnimationUtils;
import android.webkit.CookieManager;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.LinearLayout;

import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.util.Utils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URLEncoder;

/**
 * Shibboleth Authorize page
 * use cookie to get authorized data
 * <p/>
 */
public class ShibbolethAuthorizeActivity extends SherlockFragmentActivity {
    public static final String DEBUG_TAG = "ShibbolethAuthorizeActivity";

    public static final String SEAHUB_SHIB_COOKIE_NAME = "seahub_auth";
    private WebView mWebview;
    private LinearLayout mloadingAnimation;
    public String serverUrl;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.shibboleth_authorize_layout);
        mWebview = (WebView) findViewById(R.id.shibboleth_authorize_wv);
        mloadingAnimation = (LinearLayout) findViewById(R.id.shibboleth_loading_ll);

        mWebview.getSettings().setLoadsImagesAutomatically(true);
        mWebview.getSettings().setJavaScriptEnabled(true);
        mWebview.setScrollBarStyle(View.SCROLLBARS_INSIDE_OVERLAY);

        CustomWebviewClient client = new CustomWebviewClient();
        mWebview.setWebViewClient(client);

        getActionBar().setDisplayHomeAsUpEnabled(true);
        getActionBar().setTitle(R.string.shib_actionbar_title);

        String url = getIntent().getStringExtra(ShibbolethActivity.SHIBBOLETH_SERVER_URL);
        CookieManager.getInstance().removeAllCookie();
        openAuthorizePage(url);
    }

    private void openAuthorizePage(String url) {
        Log.d(DEBUG_TAG, "server url is " + url);

        serverUrl = url;

        if (!Utils.isNetworkOn()) {
            ToastUtils.show(this, getString(R.string.network_down));
            return;
        }

        String deviceId = Settings.Secure.getString(this.getContentResolver(),
                Settings.Secure.ANDROID_ID);

        String appVersion = "";
        Context context = SeadroidApplication.getAppContext();
        try {
            PackageInfo pInfo = context.getPackageManager().
                    getPackageInfo(context.getPackageName(), 0);
            appVersion = pInfo.versionName;
        } catch (PackageManager.NameNotFoundException e) {
            // ignore
        }

        if (!url.endsWith("/")) {
            url += "/shib-login";
        } else
            url += "shib-login";

        try {
            url += String.format("?shib_platform_version=%s&shib_device_name=%s&shib_platform=%s&shib_device_id=%s&shib_client_version=%s",
                    URLEncoder.encode(Build.VERSION.RELEASE, "UTF-8"),
                    URLEncoder.encode(Build.MODEL, "UTF-8"),
                    "android",
                    URLEncoder.encode(deviceId, "UTF-8"),
                    appVersion);
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }


        Log.d(DEBUG_TAG, "url " + url);

        mWebview.loadUrl(url);

        showPageLoading(true);
    }


    private void showPageLoading(boolean pageLoading) {

        if (!pageLoading) {
            mloadingAnimation.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_out));
            mWebview.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_in));
            mloadingAnimation.setVisibility(View.GONE);
            mWebview.setVisibility(View.VISIBLE);
        } else {
            mloadingAnimation.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_in));
            mWebview.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_out));

            mloadingAnimation.setVisibility(View.VISIBLE);
            mWebview.setVisibility(View.INVISIBLE);
        }
    }

    class CustomWebviewClient extends WebViewClient {
        @Override
        public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
            // Display error messages
            ToastUtils.show(ShibbolethAuthorizeActivity.this,
                    String.format((R.string.shib_load_page_error) + description));

            showPageLoading(false);
        }

        /*@Override
        public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
            Log.d(DEBUG_TAG, "onReceivedSslError " + error.getCertificate().toString());

            // Ignore SSL certificate validate
            handler.proceed();
        }*/

        @Override
        public void onPageFinished(WebView webView, String url) {
            Log.d(DEBUG_TAG, "onPageFinished " + serverUrl);
            showPageLoading(false);

            String cookie = getCookie(serverUrl, SEAHUB_SHIB_COOKIE_NAME);
            if (cookie == null)
                return;

            Account account = null;
            try {
                account = parseAccount(Utils.cleanServerURL(serverUrl), cookie);
            } catch (MalformedURLException e) {
                Log.e(DEBUG_TAG, e.getMessage());
            }
            returnAccount(account);
        }
    }

    private void returnAccount(Account account) {
        if (account == null)
            finish();

        Intent retData = new Intent();
        retData.putExtras(getIntent());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_NAME, account.getSignature());
        retData.putExtra(android.accounts.AccountManager.KEY_AUTHTOKEN, account.getToken());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_TYPE, getIntent().getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE));
        retData.putExtra(SeafileAuthenticatorActivity.ARG_EMAIL, account.getEmail());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI, account.getServer());

        // pass auth result back to the ShibbolethActivity
        setResult(RESULT_OK, retData);
        finish();
    }

    /**
     * The cookie value is like seahub_shib="foo@test.com@bd8cc1138", where
     * foo@test.com is username and bd8cc1138 is api token"
     */
    private Account parseAccount(String url, String cookie) {
        if (url == null || cookie.isEmpty())
            return null;

        if (cookie.startsWith("\"")) {
            cookie = cookie.substring(1, cookie.length() - 1);
        }

        String email = cookie.substring(0, cookie.lastIndexOf("@"));
        String token = cookie.substring(cookie.lastIndexOf("@") + 1);

        if (email.isEmpty() || token.isEmpty())
            return null;

        Log.d(DEBUG_TAG, "email: " + email);
        Log.d(DEBUG_TAG, "token: " + token);

        return new Account(url, email, token);
    }

    public String getCookie(String url, String key) {
        String CookieValue = "";

        CookieManager.getInstance().setAcceptCookie(true);
        CookieManager cookieManager = CookieManager.getInstance();

        String cookies = cookieManager.getCookie(url);
        if (cookies == null)
            return null;

        Log.d(DEBUG_TAG, "All the cookies in a string:" + cookies);

        String[] allCookies = cookies.split(";");

        for (String cookie : allCookies) {
            if (cookie.contains(key)) {
                String[] pair = cookie.split("=");
                CookieValue = pair[1];
            }
        }
        return CookieValue;
    }

}
