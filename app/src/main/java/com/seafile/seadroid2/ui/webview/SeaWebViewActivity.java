package com.seafile.seadroid2.ui.webview;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
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
import com.seafile.seadroid2.databinding.ActivitySeaWebviewBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarProgressBarBinding;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

public class SeaWebViewActivity extends BaseActivity {
    private ActivitySeaWebviewBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;

    private SeaWebView mWebView;

    private String targetUrl;

    public static void openUrlDirectly(Context context, String url) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
        intent.putExtra("targetUrl", url);
        ActivityUtils.startActivity(intent);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("targetUrl", targetUrl);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySeaWebviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        toolBinding = ToolbarActionbarProgressBarBinding.bind(binding.toolProgressBar.getRoot());

        if (savedInstanceState != null) {
            targetUrl = savedInstanceState.getString("targetUrl");
        } else {
            Intent intent = getIntent();
            targetUrl = intent.getStringExtra("targetUrl");
            if (TextUtils.isEmpty(targetUrl)) {
                throw new IllegalArgumentException("targetUrl is empty");
            }
        }

        initUI();

        //let's go
        mWebView.loadDirectly(targetUrl);
    }


    private void initUI() {
        Toolbar toolbar = toolBinding.toolbarActionbar;
        toolbar.setTitle("");
        setSupportActionBar(toolbar);
        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

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
