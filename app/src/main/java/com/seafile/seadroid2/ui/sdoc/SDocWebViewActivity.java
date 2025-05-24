package com.seafile.seadroid2.ui.sdoc;

import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.webkit.ConsoleMessage;
import android.webkit.ValueCallback;
import android.webkit.WebChromeClient;
import android.webkit.WebView;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.util.Consumer;
import androidx.core.widget.NestedScrollView;
import androidx.lifecycle.Observer;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewFeature;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.github.lzyzsd.jsbridge.CallBackFunction;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewProBinding;
import com.seafile.seadroid2.enums.WebViewPreviewType;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.docs_comment.DocsCommentsActivity;
import com.seafile.seadroid2.ui.file_profile.FileProfileDialog;
import com.seafile.seadroid2.ui.sdoc.outline.SDocOutlineDialog;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

public class SDocWebViewActivity extends BaseActivityWithVM<SDocViewModel> {
    private final String TAG = "SDocWebViewActivity";

    private ActivitySeaWebviewProBinding binding;

    private SeaWebView mWebView;
    private String repoId;
    private String path;
    private String targetUrl;

    private SDocPageOptionsModel pageOptionsData;

    public static void openSdoc(Context context, String repoName, String repoID, String path) {
        Intent intent = new Intent(context, SDocWebViewActivity.class);
        intent.putExtra("previewType", WebViewPreviewType.SDOC.name());
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", path);
        ActivityUtils.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = ActivitySeaWebviewProBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        if (!NetworkUtils.isConnected()) {
            ToastUtils.showLong(R.string.network_unavailable);
            finish();
            return;
        }

        init();

        initUI();

        initViewModel();

        //let's go
        mWebView.load(targetUrl);
    }

    private void init() {
        Intent intent = getIntent();

        if (!intent.hasExtra("previewType")) {
            throw new IllegalArgumentException("need a previewType param");
        }

        String previewType = intent.getStringExtra("previewType");
        if (!WebViewPreviewType.contains(previewType)) {
            throw new IllegalArgumentException("need a previewType param");
        }

        WebViewPreviewType previewTypeEnum = WebViewPreviewType.valueOf(previewType);

        if (previewTypeEnum == WebViewPreviewType.SDOC) {

            String repoName = intent.getStringExtra("repoName");
            repoId = intent.getStringExtra("repoID");
            path = intent.getStringExtra("filePath");

            if (TextUtils.isEmpty(repoId) || TextUtils.isEmpty(path)) {
                throw new IllegalArgumentException("repoId or path is null");
            }

            Account account = SupportAccountManager.getInstance().getCurrentAccount();
            if (account != null) {
                targetUrl = account.server + "lib/" + repoId + "/file" + path;
            } else {
                throw new IllegalArgumentException("no login");
            }
        } else {
            throw new IllegalArgumentException("previewType is not SDOC");
        }
    }

    private void initUI() {
        Toolbar toolbar = getActionBarToolbar();
//        toolbar.setTitle("");
        toolbar.setNavigationOnClickListener(v -> {
            finish();
        });

        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(null);
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

        binding.sdocOutline.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showOutlineDialog();
            }
        });
        binding.sdocProfile.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                getViewModel().loadFileDetail(repoId, path);
            }
        });
        binding.sdocComment.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showCommentsActivity();
            }
        });


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

    private void showOutlineDialog() {
        readSDocOutlineList(new Consumer<String>() {
            @Override
            public void accept(String s) {
                SDocOutlineDialog dialog = SDocOutlineDialog.newInstance(s);
                dialog.setOnItemClickListener(new OnItemClickListener<OutlineItemModel>() {
                    @Override
                    public void onItemClick(OutlineItemModel outlineItemModel, int position) {

                        callJsOutline(outlineItemModel);
                    }
                });
                dialog.show(getSupportFragmentManager(), SDocOutlineDialog.class.getSimpleName());
            }
        });
    }

    private void callJsOutline(OutlineItemModel outlineItemModel) {
        String param = GsonUtils.toJson(outlineItemModel);
        mWebView.callJsFunction("sdoc.outline.data.select", param, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "callJsOutline()", data);
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
                    ToastUtils.showShort(R.string.unknow_error);
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
                    ToastUtils.showShort(R.string.empty_data);
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
                    ToastUtils.showShort("outline is empty.");
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

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
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
//            toolBinding.toolbarActionbar.setTitle(title);
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
                    default:
                        SLogs.e("web default log: line: " + consoleMessage.lineNumber() + ", message: " + consoleMessage.message());
                        break;
                }
            }
            return super.onConsoleMessage(consoleMessage);
        }
    };

    private void setBarProgress(int p) {
        binding.toolProgressBar.setProgress(p, true);

        if (p != 100) {
            if (binding.toolProgressBar.getVisibility() != View.VISIBLE) {
                binding.toolProgressBar.setVisibility(View.VISIBLE);
            }
        }

        hideProgressBar();
    }

    private void hideProgressBar() {
        if (curProgress == 100) {
            binding.toolProgressBar.setVisibility(View.GONE);
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
