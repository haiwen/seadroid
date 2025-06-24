package com.seafile.seadroid2.ui.sdoc;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Menu;
import android.view.MenuItem;
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
import com.github.lzyzsd.jsbridge.CallBackFunction;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.WebViewActionConstant;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewProBinding;
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
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

public class SDocWebViewActivity extends BaseActivityWithVM<SDocViewModel> {
    private final String TAG = "SDocWebViewActivity";

    private ActivitySeaWebviewProBinding binding;

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

        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_unavailable);
            finish();
            return;
        }

        initData();

        initUI();

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

        NestedScrollView.LayoutParams ll = new NestedScrollView.LayoutParams(-1, -1);
        mWebView.setLayoutParams(ll);
        binding.nsv.addView(mWebView);

        if (isSdoc) {
            binding.llBottomBar.setVisibility(View.VISIBLE);
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
        } else {
            binding.llBottomBar.setVisibility(View.GONE);
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
    private boolean isEditMode = false;

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        if (!isSdoc) {
            return super.onCreateOptionsMenu(menu);
        }

        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_sdoc_preview, menu);
        editMenuItem = menu.findItem(R.id.sdoc_edit);
        editMenuItem.setVisible(true);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (item.getItemId() == R.id.sdoc_edit) {
            callJsSdocEditorEnitable(isEditMode);
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


    private void callJsGetOutline(OutlineItemModel outlineItemModel) {
        String param = GsonUtils.toJson(outlineItemModel);
        mWebView.callJsFunction(WebViewActionConstant.CallJsFunction.SDOC_OUTLINES_DATA_GET, param, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "callJsGetOutline()", data);
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


    private void callJsSdocEditorEnitable(boolean startEdit) {
        String data = String.valueOf(startEdit);
        mWebView.callJsFunction(WebViewActionConstant.CallJsFunction.SDOC_EDITOR_DATA_EDIT, data, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "callJsSdocEditorEnitable()", data);
                if (TextUtils.isEmpty(data)) {
                    Toasts.showShort(R.string.unknow_error);
                    return;
                }
                if (!data.contains("success")) {
                    Toasts.showShort(R.string.unknow_error);
                    return;
                }

                isEditMode = !isEditMode;
                if (isEditMode) {
                    editMenuItem.setTitle(R.string.edit);
                } else {
                    editMenuItem.setTitle(R.string.complete);
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
    // Mozilla/5.0 (Linux; Android 13; Pixel 7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.5735.61 Mobile Safari/537.36

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
    public void onDestroy() {
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
