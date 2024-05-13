package com.seafile.seadroid2.ui.webview;

import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;

import com.blankj.utilcode.util.ActivityUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

public class SeaWebViewActivity extends BaseActivity {
    private LinearLayout mContainer;
    private SeaWebView mWebView;
    private ProgressBar mProgressBar;
    private Toolbar toolbar;

    private String targetUrl;
    private String mRepoName, mRepoID, mFilePath;


    public static void openSdoc(Context context, String repoName, String repoID, String path) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", path);
        ActivityUtils.startActivity(intent);
    }

    public static void openUrl(Context context, String url) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
        intent.putExtra("targetUrl", url);
        ActivityUtils.startActivity(intent);
    }

    public static void openUrlDirectly(Context context, String url) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
        intent.putExtra("targetUrl", url);
        intent.putExtra("isDirect", true);
        ActivityUtils.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_sea_webview);

        Intent intent = getIntent();

        targetUrl = intent.getStringExtra("targetUrl");
        mRepoName = intent.getStringExtra("repoName");
        mRepoID = intent.getStringExtra("repoID");
        mFilePath = intent.getStringExtra("filePath");

        if (TextUtils.isEmpty(targetUrl) && TextUtils.isEmpty(mFilePath)) {
            throw new IllegalArgumentException("load url is null");
        } else if (!TextUtils.isEmpty(mFilePath)) {
            if (TextUtils.isEmpty(mRepoID)) {
                throw new IllegalArgumentException("repoId is null");
            }

            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account != null) {
                targetUrl = buildSdocUrl(account.server);
            } else {
                throw new IllegalArgumentException("no login");
            }
        }

        initOnBackPressedDispatcher();

        initUI();

        //let's go
        if (intent.hasExtra("isDirect") && intent.getBooleanExtra("isDirect", false)) {
            mWebView.loadDirectly(targetUrl);
        } else {
            mWebView.load(targetUrl);
        }
    }

    private String buildSdocUrl(String server) {
        return server + "lib/" + mRepoID + "/file" + mFilePath;
    }

    private void initUI() {
        mContainer = findViewById(R.id.container);
        mProgressBar = findViewById(R.id.tool_progress_bar);

        toolbar = findViewById(R.id.toolbar_actionbar);
        toolbar.setTitle("");
        setSupportActionBar(toolbar);
        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

        if (checkRemoveDownloadMenuConfig()) {
            toolbar.getMenu().removeItem(R.id.download);
        }

        mWebView = PreloadWebView.getInstance().getWebView(this);
        LinearLayout.LayoutParams ll = new LinearLayout.LayoutParams(-1, -1);
        ll.weight = 1;
        mWebView.setLayoutParams(ll);
        mContainer.addView(mWebView);

        //chrome client
        mWebView.setWebChromeClient(mWebChromeClient);

    }

    private boolean checkRemoveDownloadMenuConfig() {
        if (!targetUrl.startsWith("http")) {
            //remove
            return true;
        }

        if (!TextUtils.isEmpty(mFilePath) && !mFilePath.endsWith(Constants.Format.SDOC)) {
            //In the current version, it is only allowed to download files in sdoc format, which can be modified later to meet other needs.
            return true;
        }
        return false;
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
    }

    private void initOnBackPressedDispatcher() {
        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (mWebView != null && mWebView.canGoBack()) {
                    mWebView.goBack();
                } else {
                    finish();
                }
            }
        });
    }

//    @Override
//    public boolean onCreateOptionsMenu(@NonNull Menu menu) {
//        getMenuInflater().inflate(R.menu.menu_sea_webview, menu);
//        return true;
//    }
//
//    @Override
//    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
//        if (item.getItemId() == R.id.download) {
//            download();
//        }
//        return super.onOptionsItemSelected(item);
//    }


    private final WebChromeClient mWebChromeClient = new WebChromeClient() {
        @Override
        public void onProgressChanged(WebView view, int newProgress) {
            super.onProgressChanged(view, newProgress);
            setBarProgress(newProgress);
        }

        @Override
        public void onReceivedTitle(WebView view, String title) {
            super.onReceivedTitle(view, title);
            if (toolbar != null) {
                toolbar.setTitle(title);
            }
        }
    };

    private void setBarProgress(int p) {
        mProgressBar.setProgress(p, true);

        if (p != 100) {
            if (mProgressBar.getVisibility() != View.VISIBLE) {
                mProgressBar.setVisibility(View.VISIBLE);
            }
        } else {
            mProgressBar.setVisibility(View.GONE);
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (mWebView != null) {
            mWebView.onPause();
        }
    }

    @Override
    protected void onStart() {
        super.onStart();
        if (mWebView != null) {
            mWebView.onResume();
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        destroyWebView();

    }

    public void destroyWebView() {
        if (mWebView == null) {
            return;
        }

        mContainer.removeView(mWebView);

        mWebView.loadUrl("about:blank");
        mWebView.stopLoading();
        mWebView.destroy();
    }
}
