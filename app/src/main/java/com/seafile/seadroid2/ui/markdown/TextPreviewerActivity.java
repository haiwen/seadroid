package com.seafile.seadroid2.ui.markdown;

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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Objects;

/**
 * Activity for previewing plain text files with WebView
 */
public class TextPreviewerActivity extends BaseActivityWithVM<EditorViewModel> {
    private static final String TAG = "TextPreviewActivity";
    private static final String PREVIEW_URL = "https://preview.local/text";
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
            finish();
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
                if (request == null || request.getUrl() == null || !request.isForMainFrame()) {
                    return build404Response();
                }

                if (!PREVIEW_URL.equalsIgnoreCase(request.getUrl().toString())) {
                    return build404Response();
                }

                File file = path != null ? new File(path) : null;
                if (file == null || !file.exists() || !file.isFile()) {
                    view.post(() -> Toasts.show(R.string.file_not_found));
                    return build404Response();
                }

                try {
                    InputStream source = new BufferedInputStream(new FileInputStream(file));
                    InputStream stream = new EscapedHtmlInputStream(source, css);
                    return new WebResourceResponse("text/html", "UTF-8", stream);
                } catch (Exception e) {
                    SafeLogs.e(TAG, "shouldInterceptRequest failed to read file", e.getMessage());
                    view.post(() -> Toasts.show(R.string.file_not_found));
                    return build404Response();
                }
            }
        });

        binding.webView.loadUrl(PREVIEW_URL);
    }

    @NonNull
    private WebSettings getWebSettings() {
        WebSettings webSettings = binding.webView.getSettings();
        webSettings.setJavaScriptEnabled(false);
        webSettings.setAllowFileAccess(false);
        webSettings.setAllowContentAccess(false);
        webSettings.setAllowFileAccessFromFileURLs(false);
        webSettings.setAllowUniversalAccessFromFileURLs(false);

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

    private static class EscapedHtmlInputStream extends InputStream {
        private final byte[] head;
        private final byte[] tail;
        private final InputStream source;
        private int headOffset = 0;
        private int tailOffset = 0;
        private boolean sourceEnded = false;
        private byte[] pendingBytes = null;
        private int pendingOffset = 0;
        private final byte[] singleByteBuffer = new byte[1];

        private static final byte[] AMP = "&amp;".getBytes(StandardCharsets.UTF_8);
        private static final byte[] LT = "&lt;".getBytes(StandardCharsets.UTF_8);
        private static final byte[] GT = "&gt;".getBytes(StandardCharsets.UTF_8);

        public EscapedHtmlInputStream(InputStream source, String css) {
            this.source = source;
            String headStr = "<!DOCTYPE html><html><head>"
                    + "<meta charset=\"utf-8\">"
                    + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=yes\">"
                    + "<style>" + css + "</style></head><body><div class=\"content\">";
            this.head = headStr.getBytes(StandardCharsets.UTF_8);
            this.tail = "</div></body></html>".getBytes(StandardCharsets.UTF_8);
        }

        @Override
        public int read() throws IOException {
            int n = read(singleByteBuffer, 0, 1);
            return n == 1 ? singleByteBuffer[0] & 0xFF : -1;
        }

        @Override
        public int read(@NonNull byte[] b, int off, int len) throws IOException {
            Objects.requireNonNull(b);
            if (off < 0 || len < 0 || len > b.length - off) {
                throw new IndexOutOfBoundsException();
            } else if (len == 0) {
                return 0;
            }

            int bytesRead = 0;

            // 1. Drain head if still remaining
            if (headOffset < head.length) {
                int toCopy = Math.min(len, head.length - headOffset);
                System.arraycopy(head, headOffset, b, off, toCopy);
                headOffset += toCopy;
                bytesRead += toCopy;
                if (bytesRead == len) {
                    return bytesRead;
                }
            }

            // 2. Drain pending escaped bytes if any
            if (pendingBytes != null) {
                int available = pendingBytes.length - pendingOffset;
                int toCopy = Math.min(len - bytesRead, available);
                System.arraycopy(pendingBytes, pendingOffset, b, off + bytesRead, toCopy);
                pendingOffset += toCopy;
                bytesRead += toCopy;
                if (pendingOffset >= pendingBytes.length) {
                    pendingBytes = null;
                    pendingOffset = 0;
                }
                if (bytesRead == len) {
                    return bytesRead;
                }
            }

            // 3. Read from source stream
            while (!sourceEnded && bytesRead < len) {
                int next = source.read();
                if (next == -1) {
                    sourceEnded = true;
                    break;
                }
                if (next == '&') {
                    pendingBytes = AMP;
                } else if (next == '<') {
                    pendingBytes = LT;
                } else if (next == '>') {
                    pendingBytes = GT;
                } else {
                    b[off + bytesRead++] = (byte) next;
                    continue;
                }

                int toCopy = Math.min(len - bytesRead, pendingBytes.length);
                System.arraycopy(pendingBytes, 0, b, off + bytesRead, toCopy);
                bytesRead += toCopy;
                if (toCopy < pendingBytes.length) {
                    pendingOffset = toCopy;
                } else {
                    pendingBytes = null;
                    pendingOffset = 0;
                }
            }

            // 4. Drain tail if source ended and there are no pending bytes
            if (sourceEnded && pendingBytes == null && bytesRead < len && tailOffset < tail.length) {
                int toCopy = Math.min(len - bytesRead, tail.length - tailOffset);
                System.arraycopy(tail, tailOffset, b, off + bytesRead, toCopy);
                tailOffset += toCopy;
                bytesRead += toCopy;
            }

            return bytesRead > 0 ? bytesRead : -1;
        }

        @Override
        public void close() throws IOException {
            source.close();
        }
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
        return "body, pre, .content {"
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

