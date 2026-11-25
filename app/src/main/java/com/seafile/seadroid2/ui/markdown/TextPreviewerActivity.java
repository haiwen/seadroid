package com.seafile.seadroid2.ui.markdown;

import android.annotation.SuppressLint;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.NonNull;
import androidx.core.content.FileProvider;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewFeature;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.ActivityTextPreviewBinding;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.util.MimeUtils;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.editor.EditorActivity;
import com.seafile.seadroid2.ui.editor.EditorViewModel;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

/**
 * Activity for previewing plain text files with WebView
 */
public class TextPreviewerActivity extends BaseActivityWithVM<EditorViewModel> {
    private static final String TAG = "TextPreviewActivity";
    private ActivityTextPreviewBinding binding;
    private String path, repoId, fullPathInRemote;
    private PermissionEntity repoPermission;

    public static void start(Context context, String localPath, String repoId, String target_file) {
        Intent starter = new Intent(context, TextPreviewerActivity.class);
        starter.putExtra("local_path", localPath);
        starter.putExtra("full_path", target_file);
        starter.putExtra("repo_id", repoId);
        context.startActivity(starter);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("local_path", path);
        outState.putString("repo_id", repoId);
        outState.putString("full_path", fullPathInRemote);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityTextPreviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        initToolbar();

        applyEdgeToEdge(binding.getRoot());

        if (savedInstanceState != null) {
            path = savedInstanceState.getString("local_path");
            repoId = savedInstanceState.getString("repo_id");
            fullPathInRemote = savedInstanceState.getString("full_path");
        } else {
            Intent intent = getIntent();
            path = intent.getStringExtra("local_path");
            repoId = intent.getStringExtra("repo_id");
            fullPathInRemote = intent.getStringExtra("full_path");
        }

        if (path == null) {
            return;
        }

        initWebView();

        getActionBarToolbar().setNavigationOnClickListener(v -> getOnBackPressedDispatcher().onBackPressed());
        if (getSupportActionBar() != null) {
            getSupportActionBar().setTitle(FileUtils.getFileName(path));
        }

        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_unavailable);
        }

        load();
    }

    private void initWebView() {
        WebSettings webSettings = getWebSettings();

        int mode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        boolean isNight = mode == Configuration.UI_MODE_NIGHT_YES;
        if (isNight && WebViewFeature.isFeatureSupported(WebViewFeature.FORCE_DARK)) {
            WebSettingsCompat.setForceDark(webSettings, WebSettingsCompat.FORCE_DARK_ON);
        }

        String css = getCss(isNight);

        binding.webView.setWebViewClient(new WebViewClient() {
            @Override
            public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {
                if (request == null) {
                    return build404Response();
                }

                if (request.getUrl() == null) {
                    return build404Response();
                }

                if (!request.isForMainFrame()) {
                    return build404Response();
                }

                Uri reqUri = request.getUrl();

                if (!"file".equalsIgnoreCase(reqUri.getScheme())) {
                    return build404Response();
                }

                String filePath = reqUri.getPath();
                File file = filePath != null ? new File(filePath) : null;
                if (file == null || !file.exists() || !file.isFile() || !isTargetPath(filePath)) {                    view.post(() -> Toasts.show(R.string.file_not_found));
                    return build404Response();
                }

                try {
                    return new WebResourceResponse("text/plain", "UTF-8", new FileInputStream(file));
                } catch (Exception e) {
                    SafeLogs.e(TAG, "shouldInterceptRequest failed to read file", e.getMessage());
                    view.post(() -> Toasts.show(R.string.file_not_found));
                    return build404Response();
                }
            }

            @Override
            public void onPageFinished(WebView view, String url) {
                super.onPageFinished(view, url);
                String js = "javascript:(function() {"
                        + "var meta = document.querySelector('meta[name=viewport]');"
                        + "if (!meta) {"
                        + "  meta = document.createElement('meta');"
                        + "  meta.name = 'viewport';"
                        + "  meta.content = 'width=device-width, initial-scale=1.0, user-scalable=yes';"
                        + "  document.head.appendChild(meta);"
                        + "}"
                        + "var style = document.createElement('style');"
                        + "style.innerHTML = '" + css + "';"
                        + "document.head.appendChild(style);"
                        + "})()";
                view.evaluateJavascript(js, null);
            }
        });

        Uri uri = Uri.fromFile(new File(path));
        binding.webView.loadUrl(uri.toString());
    }

    @SuppressLint("SetJavaScriptEnabled")
    @NonNull
    private WebSettings getWebSettings() {
        WebSettings webSettings = binding.webView.getSettings();
        webSettings.setJavaScriptEnabled(true);
        webSettings.setAllowFileAccess(true);
        webSettings.setAllowContentAccess(false);

        webSettings.setDefaultTextEncodingName("UTF-8");
        webSettings.setUseWideViewPort(false);
        webSettings.setLoadWithOverviewMode(false);

        webSettings.setTextZoom(100);
        webSettings.setDefaultFontSize(16);
        webSettings.setDefaultFixedFontSize(15);

        webSettings.setSupportZoom(true);
        webSettings.setBuiltInZoomControls(true);
        webSettings.setDisplayZoomControls(false);
        return webSettings;
    }

    private boolean isTargetPath(String requestedPath) {
        if (this.path == null || requestedPath == null) {
            return false;
        }
        return new File(this.path).getAbsolutePath().equals(new File(requestedPath).getAbsolutePath());
    }

    private WebResourceResponse build404Response() {
        return new WebResourceResponse(
                "text/plain",
                "UTF-8",
                404,
                "Not Found",
                Collections.emptyMap(),
                new ByteArrayInputStream("File not found or failed to read.".getBytes(StandardCharsets.UTF_8))
        );
    }

    @NonNull
    private String getCss(boolean isNight) {
        String bgColor = isNight ? "#1c1b1f" : "#ffffff";
        String textColor = isNight ? "#ffffff" : "#212121";
        return "body, pre {"
                + "margin: 0 !important;"
                + "padding: 8px !important;"
                + "font-family: monospace, sans-serif !important;"
                + "font-size: 14px !important;"
                + "line-height: 1.2 !important;"
                + "white-space: pre-wrap !important;"
                + "word-break: break-all !important;"
                + "overflow-wrap: break-word !important;"
                + "box-sizing: border-box !important;"
                + "background-color: " + bgColor + " !important;"
                + "color: " + textColor + " !important;"
                + "}";
    }

    private void load() {
        getViewModel().getRepoModelAndPermissionEntity(repoId, pair -> {
            if (pair == null || pair.first == null) {
                SafeLogs.e(TAG, "RepoModel is null");
                return;
            }

            repoPermission = pair.second;
            invalidateOptionsMenu();
        });
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        if (repoPermission != null && repoPermission.modify) {
            getActionBarToolbar().inflateMenu(R.menu.markdown_view_menu);
            return true;
        }
        return false;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == R.id.edit_markdown) {
            edit();
        }
        return super.onOptionsItemSelected(item);
    }

    private void edit() {
        if (repoPermission == null || !repoPermission.modify) {
            Toasts.show(R.string.share_link_no_permission);
            return;
        }

        PackageManager pm = getPackageManager();

        Intent editAsText = new Intent(Intent.ACTION_EDIT);
        Uri uri = FileProvider.getUriForFile(this, BuildConfig.FILE_PROVIDER_AUTHORITIES, new File(path));
        editAsText.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);

        String mime = MimeUtils.getMimeType(new File(path));
        editAsText.setDataAndType(uri, mime);

        if ("text/plain".equals(mime)) {
            EditorActivity.start(this, path, repoId, fullPathInRemote);
        } else if (!pm.queryIntentActivities(editAsText, 0).isEmpty()) {
            startActivity(editAsText);
        } else {
            Intent fallbackIntent = new Intent(Intent.ACTION_EDIT);
            mime = "text/plain";
            fallbackIntent.setDataAndType(uri, mime);

            try {
                startActivity(fallbackIntent);
            } catch (ActivityNotFoundException e) {
                Toasts.show(R.string.activity_not_found);
            }
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (binding != null && binding.webView != null) {
            binding.webView.loadUrl("about:blank");
            binding.webView.stopLoading();
            binding.webView.destroy();
        }
    }
}

