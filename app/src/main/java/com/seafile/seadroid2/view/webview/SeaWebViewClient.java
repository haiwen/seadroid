package com.seafile.seadroid2.view.webview;

import static com.blankj.utilcode.util.ActivityUtils.startActivity;

import android.content.Intent;
import android.net.Uri;
import android.text.TextUtils;
import android.util.Log;
import android.webkit.CookieManager;
import android.webkit.URLUtil;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.util.SeafileLog;
import com.seafile.seadroid2.util.Token2SessionConverts;
import com.seafile.seadroid2.util.URLs;

import java.util.HashMap;
import java.util.Map;

public class SeaWebViewClient extends WebViewClient {
    @Override
    public boolean shouldOverrideUrlLoading(WebView wb, WebResourceRequest request) {
        String url = request.getUrl().toString();

        //Open the System app
        if (url.startsWith("tel:") ||
                url.startsWith("sms:") ||
                url.startsWith("smsto:") ||
                url.startsWith("mailto:") ||
                url.startsWith("mms:") ||
                url.startsWith("mmsto:")) {
            Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
            startActivity(intent);
            return true;
        }

//        if (url.startsWith(SeaWebView.URL_LOGIN)) {
//            reloadUrl(url, wb);
//            return true;
//        }

        return super.shouldOverrideUrlLoading(wb, request);
    }


//    private void reloadUrl(String url, WebView wb) {
//        CookieManager manager = CookieManager.getInstance();
//        if (manager == null) {
//            go(url, wb);
//            return;
//        }
//
//        String q = URLs.getQuery(url);
//        if (!TextUtils.isEmpty(q) && q.startsWith("next=")) {
//            q = q.substring(5);
//        }
//
//        String server = SupportAccountManager.getInstance().getCurrentAccount().getProtocolHost();
//        String newUrl = server + q;
//
//        manager.removeAllCookies(value -> goWithToken(newUrl, wb, true));
//    }

    private String mOriginTargetUrl;

    public void go(String targetUrl, WebView wb) {
        goWithToken(targetUrl, wb, true);
    }

    private void goWithToken(String targetUrl, WebView wb, boolean isRedirect) {
        Log.d(getClass().getSimpleName(), "targetUrl: " + targetUrl);

        mOriginTargetUrl = targetUrl;
        Map<String, String> map = new HashMap<>();
        map.put("Authorization", "Token " + SupportAccountManager.getInstance().getCurrentAccount().token);
        wb.loadUrl(buildUrl(mOriginTargetUrl, isRedirect), map);
    }

    private String buildUrl(String url, boolean isRedirect) {
        if (!url.startsWith("http")) {
            return url;
        }

        if (isRedirect) {
            String rUrl = Token2SessionConverts.buildUrl(url);
            Log.d(getClass().getSimpleName(), "link redirect to -> " + rUrl);
            return rUrl;
        }

        // Optimise the code here:
        // The expiry time of each cookie field is not consistent,
        // and it is possible to be redirected to the login page when opening the link
        String cookieStr = SupportCookieManager.getCookie(url);
        String u = url;
        if (TextUtils.isEmpty(cookieStr)) {
            u = Token2SessionConverts.buildUrl(url);
            Log.d(getClass().getSimpleName(), "link redirect to -> " + u);
        } else {
            Log.d(getClass().getSimpleName(), "link to -> " + u);
        }
        return u;
    }

}

