package com.seafile.seadroid2.ui.activity;

import android.content.Intent;
import android.net.http.SslError;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.animation.AnimationUtils;
import android.webkit.CookieManager;
import android.webkit.SslErrorHandler;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.LinearLayout;
import android.widget.Toast;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.util.Utils;

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
        openAuthorizePage(url);
    }

    private void openAuthorizePage(String url) {
        Log.d(DEBUG_TAG, "server url is " + url);

        if (!Utils.isNetworkOn()) {
            ToastUtils.show(this, getString(R.string.network_down));
            return;
        }

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
            Toast.makeText(
                    ShibbolethAuthorizeActivity.this,
                    String.format((R.string.shib_load_page_error) + description),
                    Toast.LENGTH_SHORT
            ).show();

            showPageLoading(false);
        }

        @Override
        public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
            Log.d(DEBUG_TAG, "onReceivedSslError " + error.getCertificate().toString());

            // Ignore SSL certificate validate
            handler.proceed();
        }

        @Override
        public void onPageFinished(WebView webView, String url) {
            Log.d(DEBUG_TAG, "onPageFinished " + url);
            showPageLoading(false);

            String cookie = getCookie(url, SEAHUB_SHIB_COOKIE_NAME);
            if (cookie == null)
                return;

            Account account = parseAccount(url, cookie);
            saveAccount(account);
            openResource(account);
        }
    }

    private void openResource(Account account) {
        if (account == null)
            return;

        Intent intent = new Intent(this, AccountsActivity.class);
        startActivity(intent);
        finish();
    }

    private void saveAccount(Account account) {
        if (account == null)
            return;

        AccountManager accountManager = new AccountManager(this);

        // save account to SharedPrefs
        accountManager.saveCurrentAccount(account);

        // save account to database
        accountManager.saveAccountToDB(account);
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

        return new Account(url, email, null, token);
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
