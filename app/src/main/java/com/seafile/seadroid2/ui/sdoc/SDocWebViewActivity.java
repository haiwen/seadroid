package com.seafile.seadroid2.ui.sdoc;

import android.content.Context;
import android.content.Intent;

import android.content.res.Configuration;
import android.os.Bundle;

import android.text.TextUtils;
import android.view.View;
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
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.gms.tasks.Continuation;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewProBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarProgressBarBinding;
import com.seafile.seadroid2.enums.WebViewPreviewType;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocProfileConfigModel;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocRecordWrapperModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.sdoc.comments.SDocCommentsActivity;
import com.seafile.seadroid2.ui.sdoc.directory.SDocDirectoryDialog;
import com.seafile.seadroid2.ui.sdoc.profile.SDocProfileDialog;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

import org.json.JSONException;
import org.json.JSONObject;

import kotlin.Pair;

@Unstable
public class SDocWebViewActivity extends BaseActivityWithVM<SDocViewModel> {
    private ActivitySeaWebviewProBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;

    private SeaWebView mWebView;
    private String repoId;
    private String path;
    private String targetUrl;

    private SDocProfileConfigModel configModel;

    public static void openSdoc(Context context, String repoName, String repoID, String path) {
        Intent intent = new Intent(context, SeaWebViewActivity.class);
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

        toolBinding = ToolbarActionbarProgressBarBinding.bind(binding.toolProgressBar.getRoot());

        init();

        initUI();

        initViewModel();

        //let's go
        mWebView.load(targetUrl);

        getViewModel().initSDocConfig(repoId, path);
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

        binding.sdocDirectory.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showDirectoryDialog();
            }
        });
        binding.sdocProfile.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showProfileDialog();
            }
        });
        binding.sdocComment.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showCommentsActivity();
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

    private void initViewModel() {
        getViewModel().getFileDetailLiveData().observe(this, new Observer<SDocProfileConfigModel>() {
            @Override
            public void onChanged(SDocProfileConfigModel sDocProfileConfigModel) {
                configModel = sDocProfileConfigModel;
                hideProgressBar();
            }
        });
        getViewModel().getSdocRecordLiveData().observe(this, new Observer<SDocRecordWrapperModel>() {
            @Override
            public void onChanged(SDocRecordWrapperModel sDocRecordWrapperModel) {
                SDocProfileDialog dialog = SDocProfileDialog.newInstance(configModel.detail, sDocRecordWrapperModel, configModel.users.user_list);
                dialog.show(getSupportFragmentManager(), SDocProfileDialog.class.getSimpleName());
            }
        });
    }

    private void showDirectoryDialog() {
        getSDocConfigData(new Consumer<SDocPageOptionsModel>() {
            @Override
            public void accept(SDocPageOptionsModel model) {
                if (TextUtils.isEmpty(model.seadocServerUrl) || TextUtils.isEmpty(model.docUuid)) {
                    return;
                }

                SDocDirectoryDialog dialog = SDocDirectoryDialog.newInstance(model);
                dialog.show(getSupportFragmentManager(), SDocDirectoryDialog.class.getSimpleName());
            }
        });
    }

    private void showProfileDialog() {
        if (configModel == null) {
            return;
        }

        if (configModel.metadata.enabled) {
            getViewModel().getRecords(repoId, path);
        } else {
            SDocProfileDialog dialog = SDocProfileDialog.newInstance(configModel.detail, configModel.users.user_list);
            dialog.show(getSupportFragmentManager(), SDocProfileDialog.class.getSimpleName());
        }
    }

    private void showCommentsActivity() {
        getSDocConfigData(new Consumer<SDocPageOptionsModel>() {
            @Override
            public void accept(SDocPageOptionsModel model) {
                SDocCommentsActivity.start(SDocWebViewActivity.this, model);
            }
        });
    }

    private void getSDocConfigData(Consumer<SDocPageOptionsModel> continuation) {
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
                SLogs.e(value);
                if (!TextUtils.isEmpty(value)) {
                    value = StringUtils.deString(value).replace("\\", "");
                    SDocPageOptionsModel configModel1 = GsonUtils.fromJson(value, SDocPageOptionsModel.class);
                    if (configModel1 != null) {
                        continuation.accept(configModel1);
                        SLogs.d("获取的 PageOption 对象数据: " + configModel1);
                    } else {
                        SLogs.d("获取的 PageOption 对象数据是空的");
                    }

                } else {
                    SLogs.d("doc uuid is empty.");
                    ToastUtils.showShort("doc uuid is empty.");
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
            toolBinding.toolbarActionbar.setTitle(title);
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
        if (configModel != null && curProgress == 100) {
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
