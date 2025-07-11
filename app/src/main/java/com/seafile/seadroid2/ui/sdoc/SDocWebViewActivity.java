package com.seafile.seadroid2.ui.sdoc;

import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.webkit.ConsoleMessage;
import android.webkit.ValueCallback;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebView;
import android.widget.LinearLayout;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.graphics.Insets;
import androidx.core.util.Consumer;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsAnimationCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.widget.NestedScrollView;
import androidx.lifecycle.Observer;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewFeature;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.blankj.utilcode.util.KeyboardUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.github.lzyzsd.jsbridge.CallBackFunction;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.WebViewActionConstant;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewProBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarProgressBarBinding;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.docs_comment.DocsCommentsActivity;
import com.seafile.seadroid2.ui.file_profile.FileProfileDialog;
import com.seafile.seadroid2.ui.sdoc.outline.SDocOutlineDialog;
import com.seafile.seadroid2.view.webview.OnWebPageListener;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

import java.util.List;

public class SDocWebViewActivity extends BaseActivityWithVM<SDocViewModel> {
    private final String TAG = "SDocWebViewActivity";

    private ActivitySeaWebviewProBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;
    private SeaWebView mWebView;
    private String repoId, repoName;
    private String path;
    private String fileName;
    private String targetUrl;
    private boolean isSdoc;

    private SDocPageOptionsModel pageOptionsData;

    public static void openSdoc(Context context, String repoName, String repoID, String path, String name) {
        Intent intent = new Intent(context, SDocWebViewActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", path);
        intent.putExtra("fileName", name);
        intent.putExtra("isSdoc", true);
        ActivityUtils.startActivity(intent);
    }

    public static void openDraw(Context context, String repoName, String repoID, String path, String name) {
        Intent intent = new Intent(context, SDocWebViewActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", path);
        intent.putExtra("fileName", name);
        intent.putExtra("isSdoc", false);
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
        outState.putString("targetUrl", targetUrl);
    }


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySeaWebviewProBinding.inflate(getLayoutInflater());
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
        adaptInputMethod();

        initViewModel();

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
        isSdoc = intent.getBooleanExtra("isSdoc", false);

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
        mWebView.setOnWebPageListener(onWebPageListener);

        NestedScrollView.LayoutParams ll = new NestedScrollView.LayoutParams(-1, -1);
        mWebView.setLayoutParams(ll);
        binding.nsv.addView(mWebView);

        if (isSdoc) {
            binding.llBottomBar.setVisibility(View.VISIBLE);
            binding.sdocOutline.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (!nextEditMode) {
                        callJsSdocEditorEnitable();
                    }

                    if (KeyboardUtils.isSoftInputVisible(SDocWebViewActivity.this)) {
                        KeyboardUtils.hideSoftInput(SDocWebViewActivity.this);
                    }

                    showOutlineDialog();
//                    callJsGetOutline();
                }
            });
            binding.sdocProfile.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (!nextEditMode) {
                        callJsSdocEditorEnitable();
                    }

                    if (KeyboardUtils.isSoftInputVisible(SDocWebViewActivity.this)) {
                        KeyboardUtils.hideSoftInput(SDocWebViewActivity.this);
                    }

                    getViewModel().loadFileDetail(repoId, path);
                }
            });
            binding.sdocComment.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (!nextEditMode) {
                        callJsSdocEditorEnitable();
                    }

                    if (KeyboardUtils.isSoftInputVisible(SDocWebViewActivity.this)) {
                        KeyboardUtils.hideSoftInput(SDocWebViewActivity.this);
                    }

                    showCommentsActivity();
                }
            });
        } else {
            binding.llBottomBar.setVisibility(View.GONE);
        }

    }

    private void adaptInputMethod() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            ViewCompat.setWindowInsetsAnimationCallback(binding.llBottomBar, new WindowInsetsAnimationCompat.Callback(WindowInsetsAnimationCompat.Callback.DISPATCH_MODE_STOP) {

                        //                        private boolean lastImeVisible = false;
                        private int startHeight = 0;
                        private int lastDiffH = 0;

                        @Override
                        public void onPrepare(@NonNull WindowInsetsAnimationCompat animation) {
                            if (startHeight == 0) {
                                startHeight = binding.llBottomBar.getHeight();
                            }
                        }

                        @NonNull
                        @Override
                        public WindowInsetsCompat onProgress(@NonNull WindowInsetsCompat insets,
                                                             @NonNull List<WindowInsetsAnimationCompat> runningAnimations) {
                            Insets imeInsets = insets.getInsets(WindowInsetsCompat.Type.ime());
                            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());

                            Insets diff = Insets.subtract(imeInsets, systemBars);
                            Insets maxDiff = Insets.max(diff, Insets.NONE);

                            int diffH = Math.abs(maxDiff.top - maxDiff.bottom);

                            ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams) binding.llBottomBar.getLayoutParams();
                            layoutParams.bottomMargin = diffH;
                            binding.llBottomBar.setLayoutParams(layoutParams);

                            lastDiffH = diffH;
                            return insets;
                        }
                    }
            );
        } else {
            // <= Android R
            binding.llBottomBar.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
                int lastBottom = 0;

                @Override
                public void onGlobalLayout() {
                    WindowInsetsCompat insets = ViewCompat.getRootWindowInsets(binding.llBottomBar);
                    if (insets != null) {
                        int bottom = insets.getInsets(WindowInsetsCompat.Type.ime()).bottom;
                        if (lastBottom != 0 && bottom == 0) {
                            binding.llBottomBar.getViewTreeObserver().removeOnGlobalLayoutListener(this);
                        }
                        lastBottom = bottom;
                    }
                }
            });
        }
    }

    private void initViewModel() {
        getViewModel().getSecondRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoadingDialog(aBoolean);
            }
        });

        getViewModel().getFileDetailLiveData().observe(this, new Observer<FileProfileConfigModel>() {
            @Override
            public void onChanged(FileProfileConfigModel fileProfileConfigModel) {
                FileProfileDialog dialog = FileProfileDialog.newInstance(fileProfileConfigModel);
                dialog.show(getSupportFragmentManager(), FileProfileDialog.class.getSimpleName());
            }
        });
    }

    private MenuItem editMenuItem;
    //
    private boolean nextEditMode = true;

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        if (!isSdoc) {
            return super.onCreateOptionsMenu(menu);
        }

        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_sdoc_preview, menu);
        editMenuItem = menu.findItem(R.id.sdoc_edit);
        editMenuItem.setVisible(true);
        if (!isPageLoaded) {
            editMenuItem.setEnabled(false);
        }
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == R.id.sdoc_edit) {
            callJsSdocEditorEnitable();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void showOutlineDialog() {
        readSDocOutlineList(new Consumer<String>() {
            @Override
            public void accept(String s) {
                SDocOutlineDialog dialog = SDocOutlineDialog.newInstance(s);
                dialog.setOnItemClickListener(new OnItemClickListener<OutlineItemModel>() {
                    @Override
                    public void onItemClick(OutlineItemModel outlineItemModel, int position) {

                        callJsSelectOutline(outlineItemModel);
                    }
                });
                dialog.show(getSupportFragmentManager(), SDocOutlineDialog.class.getSimpleName());
            }
        });
    }

    @Deprecated
    private void callJsGetOutline() {
        mWebView.callJsFunction(WebViewActionConstant.CallJsFunction.SDOC_OUTLINES_DATA_GET, null, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "callJsGetOutline()", data);
                SDocOutlineDialog dialog = SDocOutlineDialog.newInstance(data);
                dialog.setOnItemClickListener(new OnItemClickListener<OutlineItemModel>() {
                    @Override
                    public void onItemClick(OutlineItemModel outlineItemModel, int position) {

                        callJsSelectOutline(outlineItemModel);
                    }
                });
                dialog.show(getSupportFragmentManager(), SDocOutlineDialog.class.getSimpleName());
            }
        });
    }

    private void callJsSelectOutline(OutlineItemModel outlineItemModel) {
        String param = GsonUtils.toJson(outlineItemModel);
        mWebView.callJsFunction(WebViewActionConstant.CallJsFunction.SDOC_OUTLINES_DATA_SELECT, param, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "callJsSelectOutline()", data);
            }
        });
    }

    private final Handler timeoutHandler = new Handler(Looper.getMainLooper());

    //timeout callback or receive js callback
    private boolean jsCallbackReceived = true;
    private boolean isPageLoaded = false;
    private boolean didPageFinishSuccessfully = false;
    private final int timeoutDuration = 2000; // 2s
    private final Runnable timeoutRunnable = new Runnable() {
        @Override
        public void run() {
            if (!jsCallbackReceived) { // 检查回调是否已收到
                SLogs.d(TAG, "callJsSdocEditorEnitable() - Timeout occurred!");
                Toasts.showShort(R.string.not_supported_feature);
                jsCallbackReceived = true; // 标记已收到回调

                //
                if (editMenuItem != null) {
                    editMenuItem.setEnabled(false);
                }
            }
        }
    };

    private void callJsSdocEditorEnitable() {
        if (!jsCallbackReceived) {
            return;
        }

        timeoutHandler.removeCallbacks(timeoutRunnable);
        jsCallbackReceived = false;

        String data = "{\"edit\": " + nextEditMode + "}";

        // launch a timeout task
        timeoutHandler.postDelayed(timeoutRunnable, timeoutDuration);

        mWebView.callJsFunction(WebViewActionConstant.CallJsFunction.SDOC_EDITOR_DATA_EDIT, data, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "callJsSdocEditorEnitable(), receive data: ", data);
                if (TextUtils.isEmpty(data)) {
                    return;
                }

                // mark callback received
                jsCallbackReceived = true;
                timeoutHandler.removeCallbacks(timeoutRunnable);

                if (editMenuItem != null) {
                    editMenuItem.setEnabled(true);
                }

                if (!data.contains("success")) {
                    return;
                }

                if (!data.contains("true")) {
                    return;
                }

                nextEditMode = !nextEditMode;

                if (editMenuItem != null) {
                    editMenuItem.setTitle(nextEditMode ? R.string.edit : R.string.complete);
                }
            }
        });
    }

    private void showCommentsActivity() {
        readSDocPageOptionsData(new Consumer<SDocPageOptionsModel>() {
            @Override
            public void accept(SDocPageOptionsModel model) {
                DocsCommentsActivity.start(SDocWebViewActivity.this, model);
            }
        });
    }

    private void readSDocPageOptionsData(Consumer<SDocPageOptionsModel> continuation) {
        if (pageOptionsData != null && pageOptionsData.canUse()) {
            continuation.accept(pageOptionsData);
            return;
        }

        String js =
                "(function() {" +
                        "   if (window.app && window.app.pageOptions) {" +
                        "       return JSON.stringify(window.app.pageOptions);" +
                        "   } else {" +
                        "       return null;" +
                        "   }" +
                        "})();";
        mWebView.evaluateJavascript(js, new ValueCallback<String>() {
            @Override
            public void onReceiveValue(String value) {
                if (!TextUtils.isEmpty(value)) {
                    value = StringUtils.deString(value).replace("\\", "");
                    pageOptionsData = GsonUtils.fromJson(value, SDocPageOptionsModel.class);
                    if (pageOptionsData == null || !pageOptionsData.canUse()) {
                        SLogs.d(TAG, "readSDocPageOptionsData()", "read sodc page options data from web, an exception occurred in the parsing data");
                    } else {
                        continuation.accept(pageOptionsData);
                    }
                } else {
                    SLogs.d(TAG, "readSDocPageOptionsData()", "read sodc page options data from web: " + value);
                    Toasts.showShort(R.string.unknow_error);
                }
            }
        });
    }

    private void readSDocOutlineList(Consumer<String> continuation) {
        String js =
                "(function() {" +
                        "   if (window.seadroid && window.seadroid.outlines) {" +
                        "       return JSON.stringify(window.seadroid.outlines);" +
                        "   } else {" +
                        "       return null;" +
                        "   }" +
                        "})();";
        mWebView.evaluateJavascript(js, new ValueCallback<String>() {
            @Override
            public void onReceiveValue(String value) {
                if (TextUtils.isEmpty(value)) {
                    SLogs.d(TAG, "readSDocPageOptionsData()", value);
                    Toasts.showShort(R.string.empty_data);
                    continuation.accept(value);
                    return;
                }

                value = StringUtils.deStringReturnNonNull(value).replace("\\", "");
                if (continuation != null) {
                    continuation.accept(value);
                }
            }
        });
    }

    @Deprecated
    private void readSeafileTokenData(Consumer<String> continuation) {
        String js =
                "(function() {" +
                        "   if (window.seafile && window.seafile.accessToken) {" +
                        "       return JSON.stringify(window.seafile.accessToken);" +
                        "   } else {" +
                        "       return null;" +
                        "   }" +
                        "})();";
        mWebView.evaluateJavascript(js, new ValueCallback<String>() {
            @Override
            public void onReceiveValue(String value) {
                SLogs.d(TAG, "readSeafileTokenData()", value);
                if (TextUtils.isEmpty(value)) {
                    SLogs.d(TAG, "readSeafileTokenData()", "doc uuid is empty.");
                    Toasts.showShort("outline is empty.");
                    return;
                }

                value = StringUtils.deStringReturnNonNull(value).replace("\\", "");
                value = StringUtils.deStringReturnNonNull(value);

                if (continuation != null) {
                    continuation.accept(value);
                } else {

                }
            }
        });
    }

    private final OnWebPageListener onWebPageListener = new OnWebPageListener() {
        @Override
        public void onPageStarted(WebView view, String url, Bitmap favicon) {
            isPageLoaded = false;
            didPageFinishSuccessfully = false;

            if (editMenuItem != null) {
                editMenuItem.setEnabled(false);
            }
        }

        @Override
        public void onPageFinished(WebView view, String url) {
            isPageLoaded = true;

            if (!didPageFinishSuccessfully) {
                didPageFinishSuccessfully = true;
            }

            if (didPageFinishSuccessfully) {
                SLogs.d(TAG, "Page finished loading successfully: " + url);
                // 页面加载完成后，启用相关按钮
                if (editMenuItem != null) {
                    // 你可能还想在这里检查网页是否真的包含了预期的JS接口
                    // 但首先确保页面加载完成是第一步
                    editMenuItem.setEnabled(true);
                }
            } else {
                SLogs.d(TAG, "Page finished loading with errors (or was marked as error): " + url);
                // 页面加载失败，保持按钮禁用或提示用户
                if (editMenuItem != null) {
                    editMenuItem.setEnabled(false);
                }
                Toasts.showShort("网页加载失败，无法执行操作。");
            }
        }

        @Override
        public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
            if (request.isForMainFrame()) {
                isPageLoaded = true; // Page load ended (albeit wrong)
                didPageFinishSuccessfully = false;
                SLogs.e(TAG, "Error loading page: " + request.getUrl() + " Error: " + error.getDescription());
                if (editMenuItem != null) {
                    editMenuItem.setEnabled(false);
                }
            }
        }

        @Override
        public void onReceivedHttpError(WebView view, WebResourceRequest request, WebResourceResponse errorResponse) {
            if (request.isForMainFrame()) {
                isPageLoaded = true; // Page load ended (albeit wrong)
                didPageFinishSuccessfully = false;
                SLogs.e(TAG, "HTTP error loading page: " + request.getUrl() + " Status: " + errorResponse.getStatusCode());
                if (editMenuItem != null) {
                    editMenuItem.setEnabled(false);
                }
            }
        }
    };
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

    private void canLoadPageConfigData() {
        readSDocPageOptionsData(new Consumer<SDocPageOptionsModel>() {
            @Override
            public void accept(SDocPageOptionsModel model) {
                getViewModel().loadFileDetail(repoId, path);
            }
        });
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
        super.onDestroy();

        if (timeoutHandler != null) {
            timeoutHandler.removeCallbacks(timeoutRunnable);
        }

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
