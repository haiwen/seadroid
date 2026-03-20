package com.seafile.seadroid2.ui.webview;

import android.Manifest;
import android.app.DownloadManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.webkit.CookieManager;
import android.webkit.DownloadListener;
import android.webkit.WebChromeClient;
import android.webkit.WebView;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.widget.NestedScrollView;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewFeature;

import com.blankj.utilcode.util.ActivityUtils;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarProgressBarBinding;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;
import com.seafile.seadroid2.view.webview.SeaWebViewClient;

import java.net.URLDecoder;

public class SeaWebViewActivity extends BaseActivity {
    private final String TAG = "SeaWebViewActivity";
    private ActivitySeaWebviewBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;

    private SeaWebView mWebView;

    private String targetUrl;
    private boolean withToken;

    // Storage for pending download parameters
    private String pendingDownloadUrl;
    private String pendingUserAgent;
    private String pendingContentDisposition;
    private String pendingMimetype;
    private long pendingContentLength;

    private ActivityResultLauncher<String> storagePermissionLauncher;

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

        initStoragePermissionLauncher();
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

    private void download(String url, String userAgent, String contentDisposition, String mimetype, long contentLength) {
        // Check storage permission for Android 9 and below
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            // Permission granted or not needed, proceed with download
            performDownload(url, userAgent, contentDisposition, mimetype, contentLength);
        } else {
            // Save parameters for later use after permission is granted
            pendingDownloadUrl = url;
            pendingUserAgent = userAgent;
            pendingContentDisposition = contentDisposition;
            pendingMimetype = mimetype;
            pendingContentLength = contentLength;

            boolean hasPerm = checkStoragePermission();
            if (hasPerm) {
                performDownload(url, userAgent, contentDisposition, mimetype, contentLength);
            } else {
                requestStoragePermission();
            }
        }
    }

    private void performDownload(String url, String userAgent, String contentDisposition, String mimetype, long contentLength) {
        DownloadManager.Request request = new DownloadManager.Request(android.net.Uri.parse(url));
        request.addRequestHeader("User-Agent", userAgent);
        request.setMimeType(mimetype);
        SLogs.d(TAG, "url:" + url);
        SLogs.d(TAG, "contentDisposition:" + contentDisposition);
        SLogs.d(TAG, "mimetype:" + mimetype);
        SLogs.d(TAG, "contentLength:" + contentLength);
        SLogs.d(TAG, "userAgent:" + userAgent);
        // Try to extract filename from Content-Disposition header
        String fileName = extractFileNameFromContentDisposition(contentDisposition);
        if (TextUtils.isEmpty(fileName)) {
            fileName = getFileNameFromUrl(url);
        }

        // If no filename in header, extract from URL
        if (TextUtils.isEmpty(fileName)) {
            fileName = getFileNameFromUrl(url);
        }

        request.setTitle(fileName);
        request.setDescription("Downloading " + fileName);

        // Set destination - compatible with Android 10+ Scoped Storage
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            // On Android 10+, use MediaStore API to set download destination
            // or let DownloadManager choose the default location
            // Don't set destination to avoid SecurityException
        } else {
            // On Android 9 and below, use the traditional method
            request.setDestinationInExternalPublicDir(android.os.Environment.DIRECTORY_DOWNLOADS, fileName);
        }

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
        // Support multiple formats:
        // - Standard: attachment; filename="filename.ext" or attachment; filename=filename.ext
        // - RFC 5987: attachment; filename*=UTF-8''encoded_filename
        // - Non-standard: attachment; filename*=utf-8encoded_filename (missing quotes)
        String[] parts = contentDisposition.split(";");
        String rfc5987FileName = null;
        String standardFileName = null;

        for (String part : parts) {
            part = part.trim();

            // Check for RFC 5987 format: filename*=UTF-8''encoded_filename
            if (part.startsWith("filename*=")) {
                String fileNamePart = part.substring(10); // Remove "filename*="
                // Try standard RFC 5987 format first: charset 'language' encoded_filename
                String[] fileNameSegments = fileNamePart.split("'", 3);
                if (fileNameSegments.length >= 3) {
                    String charset = fileNameSegments[0];
                    String encodedFileName = fileNameSegments[2];
                    if (!TextUtils.isEmpty(encodedFileName)) {
                        try {
                            // URL decode the filename
                            rfc5987FileName = URLDecoder.decode(encodedFileName, "UTF-8");
                        } catch (Exception e) {
                            // If decoding fails, try the original value
                            rfc5987FileName = encodedFileName;
                        }
                    }
                } else if (fileNameSegments.length == 1) {
                    // Try non-standard format: filename*=utf-8encoded_filename
                    // Check if it starts with a charset followed by encoded data
                    String remaining = fileNameSegments[0];
                    // Common charsets: utf-8, UTF-8, iso-8859-1, etc.
                    String[] charsetPrefixes = {"utf-8", "UTF-8", "UTF8", "utf8",
                                                "iso-8859-1", "ISO-8859-1",
                                                "gbk", "GBK", "gb2312", "GB2312"};

                    for (String prefix : charsetPrefixes) {
                        if (remaining.toLowerCase().startsWith(prefix.toLowerCase())) {
                            String encodedFileName = remaining.substring(prefix.length());
                            if (!TextUtils.isEmpty(encodedFileName)) {
                                try {
                                    rfc5987FileName = URLDecoder.decode(encodedFileName, "UTF-8");
                                    break;
                                } catch (Exception e) {
                                    // If decoding fails, try the original value
                                    rfc5987FileName = encodedFileName;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            // Check for standard format: filename=filename.ext
            else if (part.startsWith("filename=")) {
                String fileName = part.substring(9); // Remove "filename="
                // Remove quotes if present
                if (fileName.startsWith("\"") && fileName.endsWith("\"")) {
                    fileName = fileName.substring(1, fileName.length() - 1);
                } else if (fileName.startsWith("'") && fileName.endsWith("'")) {
                    fileName = fileName.substring(1, fileName.length() - 1);
                }

                // Try to URL decode the standard filename as well
                // Some servers send URL-encoded filenames in the standard field
                try {
                    fileName = URLDecoder.decode(fileName, "UTF-8");
                } catch (Exception e) {
                    // If decoding fails, use the original filename
                }

                standardFileName = fileName;
            }
        }

        // Prefer RFC 5987 format as it supports non-ASCII characters
        if (!TextUtils.isEmpty(rfc5987FileName)) {
            return rfc5987FileName;
        }
        return standardFileName;
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

            // URL decode the filename
            try {
                fileName = URLDecoder.decode(fileName, "UTF-8");
            } catch (Exception e) {
                // If decoding fails, use the original filename
            }

            return fileName;
        } catch (Exception e) {
            return "download_" + System.currentTimeMillis();
        }
    }

    private void initStoragePermissionLauncher() {
        storagePermissionLauncher = registerForActivityResult(
                new ActivityResultContracts.RequestPermission(),
                isGranted -> {
                    if (isGranted) {
                        // Permission granted, proceed with download
                        performDownload();
                    } else {
                        // Permission denied, show message to user
                        // You could show a Toast or Snackbar here
                    }
                });
    }

    private boolean checkStoragePermission() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            // Android 10+ doesn't need storage permission for DownloadManager
            return true;
        }
        // Check for storage permission on Android 9 and below
        return ContextCompat.checkSelfPermission(this, Manifest.permission.WRITE_EXTERNAL_STORAGE)
                == PackageManager.PERMISSION_GRANTED;
    }

    private void requestStoragePermission() {
        if (storagePermissionLauncher != null) {
            storagePermissionLauncher.launch(Manifest.permission.WRITE_EXTERNAL_STORAGE);
        }
    }

    private void performDownload() {
        // This method is called after permission is granted
        // Uses the stored pending parameters
        performDownload(pendingDownloadUrl, pendingUserAgent, pendingContentDisposition,
                pendingMimetype, pendingContentLength);
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
