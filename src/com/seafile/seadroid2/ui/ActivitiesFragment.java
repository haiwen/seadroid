package com.seafile.seadroid2.ui;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.app.Activity;
import android.net.http.SslError;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.webkit.JavascriptInterface;
import android.webkit.JsResult;
import android.webkit.SslErrorHandler;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.FrameLayout;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragment;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafRepo;

public class ActivitiesFragment extends SherlockFragment {
    private static final String DEBUG_TAG = "ActivitiesFragment";

    private static final String ACTIVITIES_URL = "api2/html/activity";

    private BrowserActivity mActivity = null;

    private WebView webView = null;
    private FrameLayout mWebViewContainer = null;
    private View mProgressContainer;

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "ActivitiesFragment Attached");
        mActivity = (BrowserActivity)activity;
    }

    @Override
    public void onDetach() {
        mActivity = null;
        super.onDetach();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        return inflater.inflate(R.layout.activities_fragment, container, false);
    }

    @Override
    public void onPause() {
        Log.d(DEBUG_TAG, "onPause");
        super.onPause();

        if (webView != null) {
            mWebViewContainer.removeView(webView);
        }
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onActivityCreated");

        mWebViewContainer = (FrameLayout)getView().findViewById(R.id.webViewContainer);
        mProgressContainer = getView().findViewById(R.id.progressContainer);

        if (webView == null) {
            webView = new WebView(mActivity);
            initWebView();
            loadActivitiesPage();
        }

        mActivity.invalidateOptionsMenu();

        super.onActivityCreated(savedInstanceState);
    }

    @Override
    public void onResume() {
        // We add the webView on create and remove it on pause, so as to make
        // it retain state. Otherwise the webview will reload the url every
        // time the tab is switched.
        super.onResume();
        mWebViewContainer.addView(webView);
    }

    public void refreshView() {
        loadActivitiesPage();
    }

    private void initWebView() {
        webView.getSettings().setJavaScriptEnabled(true);
        webView.setWebViewClient(new MyWebViewClient());

        webView.setWebChromeClient(new MyWebChromeClient());
    }

    private void loadActivitiesPage() {
        showPageLoading(true);
        Account account = mActivity.getAccount();
        String url = account.getServer() + ACTIVITIES_URL;

        webView.addJavascriptInterface(new JSObject(), "token");
        // load dummy empty html to make "token" object available
        webView.loadData("", "text/html", null);
        webView.loadUrl(url, getExtraHeaders());
    }

    private Map<String, String> getExtraHeaders() {
        Account account = mActivity.getAccount();
        String token = "Token " + account.getToken();
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("Authorization", token);

        return headers;
    }

    private void showPageLoading(boolean pageLoading) {
        if (mActivity == null) {
            return;
        }

        if (!pageLoading) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                                    mActivity, android.R.anim.fade_out));
            webView.startAnimation(AnimationUtils.loadAnimation(
                                mActivity, android.R.anim.fade_in));
            mProgressContainer.setVisibility(View.GONE);
            webView.setVisibility(View.VISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_in));
            webView.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            webView.setVisibility(View.INVISIBLE);
        }
    }

    private void viewRepo(String repoID) {
        SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            mActivity.showToast("Couldn't find this library. It may be deleted");
            return;
        }

        NavContext nav = mActivity.getNavContext();

        nav.setRepoID(repoID);
        nav.setRepoName(repo.getName());
        nav.setDir("/", repo.getRootDirID());

        // switch to LIBRARY TAB
        mActivity.getSupportActionBar().setSelectedNavigationItem(0);
    }

    private void viewFile(String repoID, String path) {
        SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            mActivity.showToast("Couldn't find this library. It may be deleted");
            return;
        }

        NavContext nav = mActivity.getNavContext();

        nav.setRepoID(repoID);
        nav.setRepoName(repo.getName());
        nav.setDir(Utils.getParentPath(path), null);

        // switch to LIBRARY TAB
        mActivity.getSupportActionBar().setSelectedNavigationItem(0);
    }

    private class MyWebViewClient extends WebViewClient {
        // Display error messages
        @Override
        public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
            Toast.makeText(mActivity, "Error: " + description, Toast.LENGTH_SHORT).show();
        }

        // Ignore SSL certificate validate
        @Override
        public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
            handler.proceed();
        }

        @Override
        public boolean shouldOverrideUrlLoading(WebView webView, String url) {
            Log.d(DEBUG_TAG, "loading url " + url);
            String API_URL_PREFIX= "api://";
            if (!url.startsWith(API_URL_PREFIX)) {
                return false;
            }


            String req = url.substring(API_URL_PREFIX.length(), url.length());

            Pattern REPO_PATTERN = Pattern.compile("repos/([-a-f0-9]{36})/?");
            Pattern REPO_FILE_PATTERN = Pattern.compile("repo/([-a-f0-9]{36})/files/\\?p=(.+)");
            Matcher matcher;

            if ((matcher = fullMatch(REPO_PATTERN, req)) != null) {
                String repoID = matcher.group(1);
                viewRepo(repoID);

            } else if ((matcher = fullMatch(REPO_FILE_PATTERN, req)) != null) {
                String repoID = matcher.group(1);

                try {
                    String path = URLDecoder.decode(matcher.group(2), "UTF-8");
                    viewFile(repoID, path);
                } catch (UnsupportedEncodingException e) {
                    // Ignore
                }
            }

            return true;
        }

        @Override
        public void onPageFinished(WebView webView, String url) {
            if (url.startsWith("data:text/html")) {
                return;
            }
            Log.d(DEBUG_TAG, "onPageFinished " + url);
            showPageLoading(false);
        }
    }

    private static Matcher fullMatch(Pattern pattern, String str) {
        Matcher matcher = pattern.matcher(str);
        return matcher.matches() ? matcher : null;
    }

    private class JSObject {
        @JavascriptInterface
        public String toString() {
            return mActivity.getAccount().getToken();
        }
    }

    private class MyWebChromeClient extends WebChromeClient {

        // For debug js
        @Override
        public boolean onJsAlert(WebView view, String url, String message, JsResult result) {
            Log.d(DEBUG_TAG, "alert: " + message);
            return super.onJsAlert(view, url, message, result);
        }
    }

}
