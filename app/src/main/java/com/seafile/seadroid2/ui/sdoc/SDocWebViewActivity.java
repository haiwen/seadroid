package com.seafile.seadroid2.ui.sdoc;

import android.content.Context;
import android.content.Intent;
import android.content.res.ColorStateList;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.SpannableString;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.util.Pair;
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
import androidx.core.content.ContextCompat;
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
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.blankj.utilcode.util.KeyboardUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.bumptech.glide.load.ResourceDecoder;
import com.github.lzyzsd.jsbridge.CallBackFunction;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.WebViewActionConstant;
import com.seafile.seadroid2.databinding.ActivitySeaWebviewProBinding;
import com.seafile.seadroid2.databinding.LayoutSdocBottomBarBinding;
import com.seafile.seadroid2.databinding.LayoutSdocEditorBar2Binding;
import com.seafile.seadroid2.databinding.LayoutSdocEditorBarBinding;
import com.seafile.seadroid2.databinding.ToolbarActionbarProgressBarBinding;
import com.seafile.seadroid2.enums.TextTypeEnum;
import com.seafile.seadroid2.framework.db.entities.PermissionEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.WebRouteModel;
import com.seafile.seadroid2.framework.model.sdoc.FileProfileConfigModel;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.framework.model.sdoc.SDocPageOptionsModel;
import com.seafile.seadroid2.framework.model.sdoc.TextTypeModel;
import com.seafile.seadroid2.framework.model.sdoc.UndoRedoModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.listener.Callback;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.docs_comment.DocsCommentsActivity;
import com.seafile.seadroid2.ui.file_profile.FileProfileDialog;
import com.seafile.seadroid2.ui.sdoc.outline.SDocOutlineDialog;
import com.seafile.seadroid2.view.webview.OnWebDataCallback;
import com.seafile.seadroid2.view.webview.OnWebPageListener;
import com.seafile.seadroid2.view.webview.PreloadWebView;
import com.seafile.seadroid2.view.webview.SeaWebView;

import java.util.List;

public class SDocWebViewActivity extends BaseActivityWithVM<SDocViewModel> {
    private final String TAG = "SDocWebViewActivity";

    private ActivitySeaWebviewProBinding binding;
    private ToolbarActionbarProgressBarBinding toolBinding;
    private LayoutSdocBottomBarBinding bottomBarBinding;

    private LayoutSdocEditorBar2Binding editorBarBinding;
    private SeaWebView mWebView;
    private String repoId, repoName, path, fileName, targetUrl;
    private PermissionEntity repoPermission;

    private SDocPageOptionsModel pageOptionsData;

    public static void openSdoc(Context context, String repoName, String repoID, String path, String name) {
        Intent intent = new Intent(context, SDocWebViewActivity.class);
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

        binding = ActivitySeaWebviewProBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        toolBinding = ToolbarActionbarProgressBarBinding.bind(binding.toolProgressBar.getRoot());
        bottomBarBinding = LayoutSdocBottomBarBinding.bind(binding.toolBottomBar.getRoot());
        editorBarBinding = LayoutSdocEditorBar2Binding.bind(binding.toolEditorBar.getRoot());

        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_unavailable);
            finish();
            return;
        }

        applyEdgeToEdge();

        initData();

        initView();
        initDropdownView();

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

        checkRepoAndPermission();
    }

    private void checkRepoAndPermission() {
        getViewModel().getRepoModelAndPermissionEntity(repoId, new io.reactivex.functions.Consumer<Pair<RepoModel, PermissionEntity>>() {
            @Override
            public void accept(Pair<RepoModel, PermissionEntity> pair) throws Exception {
                if (pair == null || pair.first == null) {
                    SafeLogs.e(TAG, "RepoModel is null");
                    return;
                }

                SLogs.d(TAG, "checkRepoAndPermission()", pair.first.repo_name, pair.second.name);

                repoPermission = pair.second;

                //refresh toolbar menu
                invalidateOptionsMenu();
            }
        });
    }

    public void applyEdgeToEdge() {
        ViewCompat.setOnApplyWindowInsetsListener(binding.getRoot(), (v, insets) -> {
            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(systemBars.left, 0, systemBars.right, systemBars.bottom);

            LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) toolBinding.statusBarGuideline.getLayoutParams();
            lp.height = systemBars.top;
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

    private void initView() {
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
        mWebView.setOnWebDataCallback(new OnWebDataCallback() {
            @Override
            public void onCallback(WebRouteModel t) {
                updateEditorBarState(t);
            }
        });
        NestedScrollView.LayoutParams ll = new NestedScrollView.LayoutParams(-1, -1);
        mWebView.setLayoutParams(ll);
        binding.nsv.addView(mWebView);

        editorBarBinding.getRoot().setVisibility(View.GONE);
        bottomBarBinding.getRoot().setVisibility(View.VISIBLE);

        bottomBarBinding.sdocOutline.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (!nextEditMode) {
                    callJsSdocEditorEditable();
                }

                if (KeyboardUtils.isSoftInputVisible(SDocWebViewActivity.this)) {
                    KeyboardUtils.hideSoftInput(SDocWebViewActivity.this);
                }

                showOutlineDialog();
//                    callJsGetOutline();
            }
        });

        bottomBarBinding.sdocProfile.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (!nextEditMode) {
                    callJsSdocEditorEditable();
                }

                if (KeyboardUtils.isSoftInputVisible(SDocWebViewActivity.this)) {
                    KeyboardUtils.hideSoftInput(SDocWebViewActivity.this);
                }

                getViewModel().loadFileDetail(repoId, path);
            }
        });
        bottomBarBinding.sdocComment.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (!nextEditMode) {
                    callJsSdocEditorEditable();
                }

                if (KeyboardUtils.isSoftInputVisible(SDocWebViewActivity.this)) {
                    KeyboardUtils.hideSoftInput(SDocWebViewActivity.this);
                }

                showCommentsActivity();
            }
        });
    }

    private void initDropdownView() {
        editorBarBinding.editorUndoContainer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                triggerJsSdocEditorMenu(TextTypeEnum.undo);
            }
        });
        editorBarBinding.editorRedoContainer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                triggerJsSdocEditorMenu(TextTypeEnum.redo);
            }
        });
        editorBarBinding.editorUnorderedListIcon.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                triggerJsSdocEditorMenu(TextTypeEnum.unordered_list);
            }
        });
        editorBarBinding.editorOrderedListIcon.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                triggerJsSdocEditorMenu(TextTypeEnum.ordered_list);
            }
        });
        editorBarBinding.editorCheckboxIcon.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                triggerJsSdocEditorMenu(TextTypeEnum.check_list_item);
            }
        });
        editorBarBinding.editorTextStyleContainer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                boolean isVisible = KeyboardUtils.isSoftInputVisible(SDocWebViewActivity.this);
                if (isVisible) {
                    KeyboardUtils.hideSoftInput(v);
                    v.postDelayed(new Runnable() {
                        @Override
                        public void run() {
                            showCustomMenuView(editorBarBinding.editorTextStyle);
                        }
                    }, 400);
                } else {
                    showCustomMenuView(editorBarBinding.editorTextStyle);
                }
            }
        });

        editorBarBinding.editorKeyboardContainer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                KeyboardUtils.hideSoftInput(SDocWebViewActivity.this);
            }
        });
    }

    private void showCustomMenuView(View anchorView) {
        SdocDropdownPopView popupWindow = new SdocDropdownPopView(this);
        popupWindow.setSelectedTextStyleModel(selectedTextTypeModel);
        popupWindow.setCallback(new Callback<TextTypeModel>() {
            @Override
            public void callback(TextTypeModel textTypeModel) {
                triggerJsSdocEditorMenu(textTypeModel);

                updateEditorBarState(textTypeModel);
            }
        });

        View contentView = popupWindow.getContentView();
        contentView.measure(
                View.MeasureSpec.makeMeasureSpec(0, View.MeasureSpec.UNSPECIFIED),
                View.MeasureSpec.makeMeasureSpec(0, View.MeasureSpec.UNSPECIFIED)
        );

        int popupHeight = contentView.getMeasuredHeight();
        int anchorHeight = anchorView.getHeight();
        int yOffset = -(popupHeight + anchorHeight);
        int xOffset = 0;
        popupWindow.showAsDropDown(anchorView, xOffset, yOffset);
    }

    private final Runnable autoResetToolbarStatusTextRunnable = new Runnable() {
        @Override
        public void run() {
            toolBinding.sdocStatusText.setText("");
        }
    };

    //The type of text where the cursor is located
    private TextTypeModel selectedTextTypeModel;

    private void updateEditorBarState(WebRouteModel model) {
        if (model == null) {
            return;
        }

        if (TextUtils.equals(model.action, WebViewActionConstant.SDOC_EDITOR_CONTENT_SELECT)) {
            TextTypeModel typeModel = GsonUtils.fromJson(model.data, TextTypeModel.class);
            updateEditorBarState(typeModel);
        } else if (TextUtils.equals(model.action, WebViewActionConstant.SDOC_EDITOR_OPERATION_EXECUTE)) {
            updateUndoRepoEditorView();
        } else if (TextUtils.equals(model.action, WebViewActionConstant.SDOC_EDITOR_SYSTEM_EVENT)) {
            TextTypeModel typeModel = GsonUtils.fromJson(model.data, TextTypeModel.class);
            updateSdocToolbarStatusTextByEvent(typeModel.type);
        }
    }

    private void updateSdocToolbarStatusTextByEvent(String event) {
        if (TextUtils.equals("document_is_saved", event)) {
            updateSdocToolbarStatusText(getString(R.string.saved), true);
        } else if (TextUtils.equals("document_is_saving", event)) {
            updateSdocToolbarStatusText(getString(R.string.saving), false);
        } else if (TextUtils.equals("server_is_reconnected", event)) {
            updateSdocToolbarStatusText(getString(R.string.reconnected), true);
        } else if (TextUtils.equals("server_is_disconnected_reconnecting", event)) {
            updateSdocToolbarStatusText(getString(R.string.reconnecting), false);
        } else if (TextUtils.equals("server_is_not_connected_operation_will_be_sent_to_server_later", event)) {
            updateSdocToolbarStatusText("", false);
            Toasts.show(R.string.sdoc_system_event_server_is_not_connected_operation_will_be_sent_to_server_later);
        } else if (TextUtils.equals("pending_operations_exceed_limit", event)) {
            updateSdocToolbarStatusText("", false);
            Toasts.show(R.string.sdoc_system_event_pending_operations_exceed_limit);
        } else if (TextUtils.equals("token_expired_Please_refresh_the_page", event)) {
            updateSdocToolbarStatusText("", false);
            Toasts.show(R.string.sdoc_system_event_token_expired_please_refresh_the_page);
        } else if (TextUtils.equals("internal_server_exec_operations_error", event)) {
            updateSdocToolbarStatusText("", false);
            Toasts.show(R.string.sdoc_system_event_internal_server_exec_operations_error);
        } else if (TextUtils.equals("failed_to_sync_with_server_operations", event)) {
            updateSdocToolbarStatusText("", false);
            Toasts.show(R.string.sdoc_system_event_failed_to_sync_with_server_operations);
        } else if (TextUtils.equals("failed_to_execute_operation_on_server", event)) {
            updateSdocToolbarStatusText("", false);
            Toasts.show(R.string.sdoc_system_event_failed_to_execute_operation_on_server);
        }
    }

    private void updateSdocToolbarStatusText(String status, boolean autoDismiss) {
        toolBinding.sdocStatusText.setText(status);

        if (autoDismiss) {
            mainLooperHandler.removeCallbacks(autoResetToolbarStatusTextRunnable);
            mainLooperHandler.postDelayed(autoResetToolbarStatusTextRunnable, 2000);
        }
    }


    private void updateEditorBarState(TextTypeModel typeModel) {
        if (typeModel == null) {
            return;
        }

        selectedTextTypeModel = typeModel;

        updateEditorBarState();

    }

    private void updateEditorBarState() {

        // update first
        resetEditorBarState();

        if (TextTypeEnum.isTextType(selectedTextTypeModel)) {
            setEditorTextStyle();

        } else if (TextUtils.equals(TextTypeEnum.unordered_list.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorUnorderedListContainer.setChecked(true);

            int t = ContextCompat.getColor(this, R.color.fancy_orange);
            ColorStateList stateList = ColorStateList.valueOf(t);
            editorBarBinding.editorUnorderedListIcon.setImageTintList(stateList);

        } else if (TextUtils.equals(TextTypeEnum.ordered_list.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorOrderedListContainer.setChecked(true);

            int t = ContextCompat.getColor(this, R.color.fancy_orange);
            ColorStateList stateList = ColorStateList.valueOf(t);
            editorBarBinding.editorOrderedListIcon.setImageTintList(stateList);

        } else if (TextUtils.equals(TextTypeEnum.check_list_item.name(), selectedTextTypeModel.type)) {

            editorBarBinding.editorCheckboxContainer.setChecked(true);

            int t = ContextCompat.getColor(this, R.color.fancy_orange);
            ColorStateList stateList = ColorStateList.valueOf(t);
            editorBarBinding.editorCheckboxIcon.setImageTintList(stateList);

            setTextTypeOrderListEnable(false);
        } else if (TextUtils.equals("op-execute", selectedTextTypeModel.type)) {
            SLogs.e(TAG, "updateEditorBarState()", "op-execute");
        }
    }

    private void setEditorTextStyle() {
        if (TextUtils.equals(TextTypeEnum.paragraph.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_paragraph);
        } else if (TextUtils.equals(TextTypeEnum.title.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_title);
        } else if (TextUtils.equals(TextTypeEnum.subtitle.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_subtitle);
        } else if (TextUtils.equals(TextTypeEnum.header1.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_h1);
        } else if (TextUtils.equals(TextTypeEnum.header2.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_h2);
        } else if (TextUtils.equals(TextTypeEnum.header3.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_h3);
        } else if (TextUtils.equals(TextTypeEnum.header4.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_h4);
        } else if (TextUtils.equals(TextTypeEnum.header5.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_h5);
        } else if (TextUtils.equals(TextTypeEnum.header6.name(), selectedTextTypeModel.type)) {
            editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_h6);
        }
    }

    private void resetEditorBarState() {

        editorBarBinding.editorTextStyle.setImageResource(R.drawable.icon_editor_paragraph);
        editorBarBinding.editorCheckboxContainer.setChecked(false);
        editorBarBinding.editorOrderedListContainer.setChecked(false);
        editorBarBinding.editorUnorderedListContainer.setChecked(false);
//        editorBarBinding.editorTextStyleContainer.setChecked(false);

        int t = ContextCompat.getColor(this, R.color.light_black);
        ColorStateList stateList = ColorStateList.valueOf(t);
        editorBarBinding.editorOrderedListIcon.setImageTintList(stateList);
        editorBarBinding.editorUnorderedListIcon.setImageTintList(stateList);
        editorBarBinding.editorCheckboxIcon.setImageTintList(stateList);

        setTextTypeOrderListEnable(true);
    }

    private void setTextTypeOrderListEnable(boolean isEnable) {
        int color;
        if (isEnable) {
            color = ContextCompat.getColor(this, R.color.light_black);
        } else {
            color = ContextCompat.getColor(this, R.color.light_grey);
        }

        editorBarBinding.editorTextStyle.setEnabled(isEnable);
        editorBarBinding.editorUnorderedListIcon.setEnabled(isEnable);
        editorBarBinding.editorOrderedListIcon.setEnabled(isEnable);

        ColorStateList stateList = ColorStateList.valueOf(color);

        editorBarBinding.editorTextStyle.setImageTintList(stateList);
        editorBarBinding.editorUnorderedListIcon.setImageTintList(stateList);
        editorBarBinding.editorOrderedListIcon.setImageTintList(stateList);
//        editorBarBinding.editorTextArrowDown.setImageTintList(stateList);
    }


    private void triggerJsSdocEditorMenu(TextTypeEnum typeEnum) {
        TextTypeModel tm = new TextTypeModel(typeEnum.name());
        triggerJsSdocEditorMenu(tm);
    }

    private void triggerJsSdocEditorMenu(TextTypeModel typeModel) {
        String param = GsonUtils.toJson(typeModel);

        mWebView.callJsFunction(WebViewActionConstant.CallJsFunction.SDOC_TOOLBAR_MENU_TRIGGER, param, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "triggerJsSdocEditorMenu()", data);

                updateUndoRepoEditorView();
            }
        });
    }


    private void updateUndoRepoEditorView() {
        execUndoRedoListJs(new Consumer<UndoRedoModel>() {
            @Override
            public void accept(UndoRedoModel undoRedoModel) {
                if (undoRedoModel == null) {
                    return;
                }

                if (CollectionUtils.isEmpty(undoRedoModel.undos) && CollectionUtils.isEmpty(undoRedoModel.redos)) {
                    setUndoRedoEnable(false, false);
                } else if (CollectionUtils.isEmpty(undoRedoModel.undos) && !CollectionUtils.isEmpty(undoRedoModel.redos)) {
                    setUndoRedoEnable(false, true);
                } else if (!CollectionUtils.isEmpty(undoRedoModel.undos) && CollectionUtils.isEmpty(undoRedoModel.redos)) {
                    setUndoRedoEnable(true, false);
                } else if (!CollectionUtils.isEmpty(undoRedoModel.undos) && !CollectionUtils.isEmpty(undoRedoModel.redos)) {
                    setUndoRedoEnable(true, true);
                }

            }
        });
    }

    private void setUndoRedoEnable(boolean undoEnable, boolean redoEnable) {
        int enableColor = ContextCompat.getColor(this, R.color.light_black);
        int disableColor = ContextCompat.getColor(this, R.color.light_grey);
        ColorStateList enableStateList = ColorStateList.valueOf(enableColor);
        ColorStateList disableStateList = ColorStateList.valueOf(disableColor);

        if (undoEnable) {
            editorBarBinding.editorUndo.setImageTintList(enableStateList);
            editorBarBinding.editorUndo.setEnabled(true);
        } else {
            editorBarBinding.editorUndo.setImageTintList(disableStateList);
            editorBarBinding.editorUndo.setEnabled(false);
        }

        if (redoEnable) {
            editorBarBinding.editorRedo.setImageTintList(enableStateList);
            editorBarBinding.editorRedo.setEnabled(true);
        } else {
            editorBarBinding.editorRedo.setImageTintList(disableStateList);
            editorBarBinding.editorRedo.setEnabled(false);
        }
    }

    private void execUndoRedoListJs(Consumer<UndoRedoModel> continuation) {
        String js =
                "(function() {" +
                        "   if (window.seadroid && window.seadroid.history) {" +
                        "       return JSON.stringify(window.seadroid.history);" +
                        "   } else {" +
                        "       return null;" +
                        "   }" +
                        "})();";
        mWebView.evaluateJavascript(js, new ValueCallback<String>() {
            @Override
            public void onReceiveValue(String value) {
                if (TextUtils.isEmpty(value)) {
                    SLogs.d(TAG, "execUndoRedoListJs()", value);
                    Toasts.showShort(R.string.empty_data);
                    continuation.accept(null);
                    return;
                }

                value = StringUtils.deStringReturnNonNull(value).replace("\\", "");

                if (continuation != null) {
                    try {
                        UndoRedoModel undoRedoModel = GsonUtils.fromJson(value, UndoRedoModel.class);
                        continuation.accept(undoRedoModel);
                    } catch (Exception e) {
                        SLogs.e(e);
                    }
                }
            }
        });
    }

    private void adaptInputMethod() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            ViewCompat.setWindowInsetsAnimationCallback(getWindow().getDecorView(), new WindowInsetsAnimationCompat.Callback(WindowInsetsAnimationCompat.Callback.DISPATCH_MODE_STOP) {
                        private int startHeight = 0;

                        @Override
                        public WindowInsetsAnimationCompat.@org.jspecify.annotations.NonNull BoundsCompat onStart(@org.jspecify.annotations.NonNull WindowInsetsAnimationCompat animation, WindowInsetsAnimationCompat.@org.jspecify.annotations.NonNull BoundsCompat bounds) {
                            return super.onStart(animation, bounds);
                        }

                        @Override
                        public void onPrepare(@NonNull WindowInsetsAnimationCompat animation) {
                            if (startHeight == 0) {
                                startHeight = binding.llPlaceholder.getHeight();
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

                            ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams) binding.llPlaceholder.getLayoutParams();
                            layoutParams.bottomMargin = diffH;
                            binding.llPlaceholder.setLayoutParams(layoutParams);

                            return insets;
                        }

                    }
            );
        } else {
            // <= Android R
            binding.llPlaceholder.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
                int lastBottom = 0;

                @Override
                public void onGlobalLayout() {
                    WindowInsetsCompat insets = ViewCompat.getRootWindowInsets(bottomBarBinding.getRoot());
                    if (insets == null) {
                        return;
                    }

                    int bottom = insets.getInsets(WindowInsetsCompat.Type.ime()).bottom;
                    if (lastBottom != 0 && bottom == 0) {
                        bottomBarBinding.getRoot().getViewTreeObserver().removeOnGlobalLayoutListener(this);
                    }
                    lastBottom = bottom;
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
        if (repoPermission != null && repoPermission.modify) {

            // Inflate the menu; this adds items to the action bar if it is present.
            getMenuInflater().inflate(R.menu.menu_sdoc_preview, menu);
            editMenuItem = menu.findItem(R.id.sdoc_edit);
            editMenuItem.setVisible(true);

            String title = editMenuItem.getTitle().toString();
            setToolMenuTitle(title);

            if (!isPageLoaded) {
                editMenuItem.setEnabled(false);
            }
        }


        return true;
    }

    private void setToolMenuTitle(String title) {
        SpannableString spanString = new SpannableString(title);
        int blue = getColor(R.color.blue_900);
        spanString.setSpan(new ForegroundColorSpan(blue), 0, spanString.length(), 0);
        editMenuItem.setTitle(spanString);
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        if (repoPermission != null && repoPermission.modify) {
            if (item.getItemId() == R.id.sdoc_edit) {
                callJsSdocEditorEditable();
                return true;
            }
        }
        return super.onOptionsItemSelected(item);
    }

    private void showOutlineDialog() {
        execSDocOutlineListJs(new Consumer<String>() {
            @Override
            public void accept(String s) {
                getViewModel().setOutlineValue(s);

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

    private final Handler mainLooperHandler = new Handler(Looper.getMainLooper());

    //timeout callback or receive js callback
    private boolean jsCallbackReceived = true;
    private boolean isPageLoaded = false;
    private boolean didPageFinishSuccessfully = false;
    private final int timeoutDuration = 5000; // 5s
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


    private void callJsSdocEditorEditable() {
        if (!jsCallbackReceived) {
            return;
        }

        mainLooperHandler.removeCallbacks(timeoutRunnable);
        jsCallbackReceived = false;

        String data = "{\"edit\": " + nextEditMode + "}";

        // launch a timeout task
        mainLooperHandler.postDelayed(timeoutRunnable, timeoutDuration);

        mWebView.callJsFunction(WebViewActionConstant.CallJsFunction.SDOC_EDITOR_DATA_EDIT, data, new CallBackFunction() {
            @Override
            public void onCallBack(String data) {
                SLogs.d(TAG, "callJsSdocEditorEnitable(), receive data: ", data);
                if (TextUtils.isEmpty(data)) {
                    return;
                }

                // mark callback received
                jsCallbackReceived = true;
                mainLooperHandler.removeCallbacks(timeoutRunnable);

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
                    String title = getString(nextEditMode ? R.string.edit : R.string.complete);
                    setToolMenuTitle(title);
                }

                if (nextEditMode) {
                    editorBarBinding.getRoot().setVisibility(View.GONE);
                    bottomBarBinding.getRoot().setVisibility(View.VISIBLE);
                } else {
                    editorBarBinding.getRoot().setVisibility(View.VISIBLE);
                    bottomBarBinding.getRoot().setVisibility(View.GONE);
                }
            }
        });
    }

    private void showCommentsActivity() {
        readSDocPageOptionsData(new Consumer<SDocPageOptionsModel>() {
            @Override
            public void accept(SDocPageOptionsModel model) {
                if (repoPermission != null) {
                    DocsCommentsActivity.start(SDocWebViewActivity.this, model, repoPermission.modify);
                }
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

    private void execSDocOutlineListJs(Consumer<String> continuation) {
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

            SLogs.d(TAG, "Page finished loading successfully: " + url);
            if (editMenuItem != null) {
                editMenuItem.setEnabled(true);
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

        if (mainLooperHandler != null) {
            mainLooperHandler.removeCallbacks(timeoutRunnable);
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
