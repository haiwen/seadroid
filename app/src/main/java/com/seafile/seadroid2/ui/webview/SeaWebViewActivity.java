package com.seafile.seadroid2.ui.webview;

import android.app.DownloadManager;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.webkit.CookieManager;
import android.webkit.DownloadListener;
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
import com.seafile.seadroid2.view.webview.SeaWebViewClient;

import java.net.URLDecoder;

public class SeaWebViewActivity extends BaseActivity {
    private ActivitySeaWebviewBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;

    private SeaWebView mWebView;

    private String targetUrl;
    private boolean withToken;

    public static void openUrl(Context context, String url, boolean withToken) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
        intent.putExtra("targetUrl", url);
        intent.putExtra("withToken", true);

        ActivityUtils.startActivity(intent);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("targetUrl", targetUrl);
        outState.putBoolean("withToken", withToken);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySeaWebviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        applyEdgeToEdge(binding.getRoot());

        toolBinding = ToolbarActionbarProgressBarBinding.bind(binding.toolProgressBar.getRoot());

        if (savedInstanceState != null) {
            targetUrl = savedInstanceState.getString("targetUrl");
            withToken = savedInstanceState.getBoolean("withToken");
        } else {
            Intent intent = getIntent();
            targetUrl = intent.getStringExtra("targetUrl");
            if (intent.hasExtra("withToken")) {
                withToken = intent.getBooleanExtra("withToken", false);
            }
            if (TextUtils.isEmpty(targetUrl)) {
                throw new IllegalArgumentException("targetUrl is empty");
            }
        }

        initUI();

        //let's go
        if (withToken) {
            mWebView.load(targetUrl);
        } else {
            mWebView.loadDirectly(targetUrl);
        }
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
        mWebView.setOnDownloadListener(new DownloadListener() {
            @Override
            public void onDownloadStart(String url, String userAgent, String contentDisposition, String mimetype, long contentLength) {
                download(url, userAgent, contentDisposition, mimetype, contentLength);
            }
        });

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

    private void download(String url, String userAgent, String contentDisposition, String mimetype, long contentLength){
        DownloadManager.Request request = new DownloadManager.Request(android.net.Uri.parse(url));
        request.addRequestHeader("User-Agent", userAgent);
        request.setMimeType(mimetype);

        // Set title and description
        String fileName = extractFileNameFromContentDisposition(contentDisposition);
        if (TextUtils.isEmpty(fileName)) {
            fileName = getFileNameFromUrl(url);
        }

        // URL decode the filename
        try {
            fileName = URLDecoder.decode(fileName, "UTF-8");
        } catch (Exception e) {
            // If decoding fails, use the original filename
        }
        request.setTitle(fileName);
        request.setDescription("Downloading " + fileName);

        // Set destination to external public downloads directory
        request.setDestinationInExternalPublicDir(android.os.Environment.DIRECTORY_DOWNLOADS, fileName);

        // Set notification visibility
        request.setNotificationVisibility(DownloadManager.Request.VISIBILITY_VISIBLE_NOTIFY_COMPLETED);

        // Add cookies if using token authentication
        if (withToken) {
            String cookies = CookieManager.getInstance().getCookie(url);
            if (!TextUtils.isEmpty(cookies)) {
                request.addRequestHeader("Cookie", cookies);
            }
        }

        // Enqueue the download
        DownloadManager downloadManager = (DownloadManager) getSystemService(Context.DOWNLOAD_SERVICE);
        if (downloadManager != null) {
            downloadManager.enqueue(request);
        }
    }

    private String extractFileNameFromContentDisposition(String contentDisposition) {
        if (TextUtils.isEmpty(contentDisposition)) {
            return null;
        }

        // Try to extract filename from Content-Disposition header
        // Format: attachment; filename="filename.ext" or attachment; filename=filename.ext
        String[] parts = contentDisposition.split(";");
        for (String part : parts) {
            part = part.trim();
            if (part.startsWith("filename=")) {
                String fileName = part.substring(9); // Remove "filename="
                // Remove quotes if present
                if (fileName.startsWith("\"") && fileName.endsWith("\"")) {
                    fileName = fileName.substring(1, fileName.length() - 1);
                } else if (fileName.startsWith("'") && fileName.endsWith("'")) {
                    fileName = fileName.substring(1, fileName.length() - 1);
                }

                return fileName;
            }
        }
        return null;
    }

    private String getFileNameFromUrl(String url) {
        if (TextUtils.isEmpty(url)) {
            return "download";
        }

        try {
            String fileName = url.substring(url.lastIndexOf('/') + 1);
            // Remove query parameters if present
            int queryIndex = fileName.indexOf('?');
            if (queryIndex > 0) {
                fileName = fileName.substring(0, queryIndex);
            }

            // If filename is empty or doesn't contain a valid extension, use default
            if (TextUtils.isEmpty(fileName) || !fileName.contains(".")) {
                return "download_" + System.currentTimeMillis();
            }
            return fileName;
        } catch (Exception e) {
            return "download_" + System.currentTimeMillis();
        }
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
