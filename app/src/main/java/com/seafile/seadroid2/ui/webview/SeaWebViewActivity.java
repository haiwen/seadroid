package com.seafile.seadroid2.ui.webview;

import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;
import android.webkit.WebChromeClient;
import android.webkit.WebView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.widget.NestedScrollView;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewFeature;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.navigation.NavigationBarView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewBinding;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewProBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarProgressBarBinding;
import com.seafile.seadroid2.enums.WebViewPreviewType;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

public class SeaWebViewActivity extends BaseActivity {
    private ActivitySeaWebviewBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;

    private SeaWebView mWebView;

    private String targetUrl;

    public static void openUrl(Context context, String url) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
        intent.putExtra("previewType", WebViewPreviewType.URL.name());
        intent.putExtra("targetUrl", url);
        ActivityUtils.startActivity(intent);
    }

    public static void openUrlDirectly(Context context, String url) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
        intent.putExtra("previewType", WebViewPreviewType.URL.name());
        intent.putExtra("targetUrl", url);
        intent.putExtra("requireToken", true);
        ActivityUtils.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySeaWebviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        toolBinding = ToolbarActionbarProgressBarBinding.bind(binding.toolProgressBar.getRoot());

        Intent intent = getIntent();

        if (!intent.hasExtra("previewType")) {
            throw new IllegalArgumentException("need a previewType param");
        }

        String previewType = intent.getStringExtra("previewType");
        if (!WebViewPreviewType.contains(previewType)) {
            throw new IllegalArgumentException("need a previewType param");
        }

        WebViewPreviewType previewTypeEnum = WebViewPreviewType.valueOf(previewType);
        if (previewTypeEnum == WebViewPreviewType.URL) {
            targetUrl = intent.getStringExtra("targetUrl");
        } else if (previewTypeEnum == WebViewPreviewType.SDOC) {
            String repoName = intent.getStringExtra("repoName");
            String repoId = intent.getStringExtra("repoID");
            String path = intent.getStringExtra("filePath");

            if (TextUtils.isEmpty(repoId)) {
                throw new IllegalArgumentException("repoId is null");
            }

            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account != null) {
                targetUrl = account.server + "lib/" + repoId + "/file" + path;
            } else {
                throw new IllegalArgumentException("no login");
            }
        }

        initUI();

        //let's go
        if (intent.hasExtra("requireToken") && intent.getBooleanExtra("requireToken", false)) {
            mWebView.loadDirectly(targetUrl);
        } else {
            mWebView.load(targetUrl);
        }
    }


    private void initUI() {
        Toolbar toolbar = toolBinding.toolbarActionbar;
        toolbar.setTitle("");
        setSupportActionBar(toolbar);
        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

//        if (checkRemoveDownloadMenuConfig()) {
//            toolbar.getMenu().removeItem(R.id.download);
//        }

        mWebView = PreloadWebView.getInstance().getWebView(this);

        if (WebViewFeature.isFeatureSupported(WebViewFeature.ALGORITHMIC_DARKENING)) {
            WebSettingsCompat.setAlgorithmicDarkeningAllowed(mWebView.getSettings(), true);
        }

        NestedScrollView.LayoutParams ll = new NestedScrollView.LayoutParams(-1, -1);
        mWebView.setLayoutParams(ll);
        binding.nsv.addView(mWebView);

        //chrome client
        mWebView.setWebChromeClient(mWebChromeClient);

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

//    private boolean checkRemoveDownloadMenuConfig() {
//        if (!targetUrl.startsWith("http")) {
//            //remove
//            return true;
//        }
//
//        if (!TextUtils.isEmpty(mFilePath) && !mFilePath.endsWith(Constants.Format.SDOC)) {
//            //In the current version, it is only allowed to download files in sdoc format, which can be modified later to meet other needs.
//            return true;
//        }
//        return false;
//    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
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
            toolBinding.toolbarActionbar.setTitle(title);
        }
    };

    private void setBarProgress(int p) {
        toolBinding.toolProgressBar.setProgress(p, true);

        if (p != 100) {
            if (toolBinding.toolProgressBar.getVisibility() != View.VISIBLE) {
                toolBinding.toolProgressBar.setVisibility(View.VISIBLE);
            }
        } else {
            toolBinding.toolProgressBar.setVisibility(View.GONE);
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

        binding.nsv.removeView(mWebView);

        mWebView.loadUrl("about:blank");
        mWebView.stopLoading();
        mWebView.destroy();
    }
}
