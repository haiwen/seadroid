package com.seafile.seadroid2.ui.office_doc;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.webkit.ConsoleMessage;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.widget.LinearLayout;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.widget.NestedScrollView;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewFeature;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewBinding;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewProBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarProgressBarBinding;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.sdoc.SDocWebViewActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

public class OfficeDocumentWebActivity extends BaseActivity {
    private final String TAG = "OfficeDocumentWebActivity";
    private ActivitySeaWebviewBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;
    private SeaWebView mWebView;
    private String repoId, repoName, path, fileName, targetUrl;

    public static void openDocument(Context context, String repoName, String repoID, String path, String name) {
        Intent intent = new Intent(context, OfficeDocumentWebActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", path);
        intent.putExtra("fileName", name);
        ActivityUtils.startActivity(intent);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        if (mWebView != null) {
            mWebView.saveState(outState);
        }
        outState.putString("repoId", repoId);
        outState.putString("repoName", repoName);
        outState.putString("path", path);
        outState.putString("fileName", fileName);
        outState.putString("targetUrl", targetUrl);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySeaWebviewBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        toolBinding = ToolbarActionbarProgressBarBinding.bind(binding.toolProgressBar.getRoot());

        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_unavailable);
            finish();
            return;
        }

        applyEdgeToEdge();

        initData();
        initUI();

        if (savedInstanceState != null) {
            repoId = savedInstanceState.getString("repoId");
            repoName = savedInstanceState.getString("repoName");
            path = savedInstanceState.getString("path");
            targetUrl = savedInstanceState.getString("targetUrl");

            mWebView.restoreState(savedInstanceState);
        } else {
            //let's go
            mWebView.load(targetUrl);
        }
    }


    public void applyEdgeToEdge() {
        ViewCompat.setOnApplyWindowInsetsListener(binding.getRoot(), (v, insets) -> {
            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(
                    systemBars.left,
                    0,
                    systemBars.right,
                    systemBars.bottom
            );

            Insets statusBars = insets.getInsets(WindowInsetsCompat.Type.statusBars());
            LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) toolBinding.statusBarGuideline.getLayoutParams();
            lp.height = statusBars.top;
            toolBinding.statusBarGuideline.setLayoutParams(lp);
            return insets;
        });
    }


    private void initData() {
        Intent intent = getIntent();
        if (intent == null) {
            throw new IllegalArgumentException("intent is null");
        }

        repoId = intent.getStringExtra("repoID");
        repoName = intent.getStringExtra("repoName");
        path = intent.getStringExtra("filePath");
        fileName = intent.getStringExtra("fileName");

        if (TextUtils.isEmpty(repoId) || TextUtils.isEmpty(path)) {
            throw new IllegalArgumentException("repoId or path is null");
        }

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account != null) {
            targetUrl = account.server + "lib/" + repoId + "/file" + path;
        } else {
            throw new IllegalArgumentException("no login");
        }
    }


    private void initUI() {
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(fileName);
        }

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

        mWebView = PreloadWebView.getInstance().getWebView(this);

        if (WebViewFeature.isFeatureSupported(WebViewFeature.ALGORITHMIC_DARKENING)) {
            WebSettingsCompat.setAlgorithmicDarkeningAllowed(mWebView.getSettings(), true);
        }

        //chrome client
        mWebView.setWebChromeClient(mWebChromeClient);

        NestedScrollView.LayoutParams ll = new NestedScrollView.LayoutParams(-1, -1);
        mWebView.setLayoutParams(ll);
        binding.nsv.addView(mWebView);

    }


    private int curProgress = 0;
    private final WebChromeClient mWebChromeClient = new WebChromeClient() {
        @Override
        public void onProgressChanged(WebView view, int newProgress) {
            super.onProgressChanged(view, newProgress);
            curProgress = newProgress;
            setBarProgress(curProgress);
        }

        @Override
        public void onReceivedTitle(WebView view, String title) {
            super.onReceivedTitle(view, title);
            toolBinding.toolbarActionbar.setTitle(title);
        }

        @Override
        public boolean onConsoleMessage(ConsoleMessage consoleMessage) {
            if (consoleMessage != null) {
                switch (consoleMessage.messageLevel()) {
                    case ERROR:
                        SLogs.e("web e log: line: " + consoleMessage.lineNumber() + ", message: " + consoleMessage.message());
                        break;
                    case DEBUG:
                        SLogs.d("web d log: line: " + consoleMessage.lineNumber() + ", message: " + consoleMessage.message());
                        break;
                    case WARNING:
                        SLogs.w("web w log: line: " + consoleMessage.lineNumber() + ", message: " + consoleMessage.message());
                        break;
                    case TIP:
                        SLogs.i("web i log: line: " + consoleMessage.lineNumber() + ", message: " + consoleMessage.message());
                        break;
                    case LOG:
                        SLogs.e("web l log: line: " + consoleMessage.lineNumber() + ", message: " + consoleMessage.message());
                        break;
                    default:
                        SLogs.e("web default log: line: " + consoleMessage.lineNumber() + ", message: " + consoleMessage.message());
                        break;
                }
            }
            return super.onConsoleMessage(consoleMessage);
        }
    };

    private void setBarProgress(int p) {
        toolBinding.toolProgressBar.setProgress(p, true);

        if (p != 100) {
            if (toolBinding.toolProgressBar.getVisibility() != View.VISIBLE) {
                toolBinding.toolProgressBar.setVisibility(View.VISIBLE);
            }
        }

        hideProgressBar();
    }

    private void hideProgressBar() {
        if (curProgress == 100) {
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
    public void onDestroy() {

        destroyWebView();

        super.onDestroy();
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
