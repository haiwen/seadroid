package com.seafile.seadroid2.ui.base;

import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowInsets;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.core.app.ComponentActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;

/**
 * A base activity that handles common functionality in the app. This includes Action Bar tweaks.
 */
public class BaseActivity extends AppCompatActivity {

    // Primary toolbar and drawer toggle
    private Toolbar mActionBarToolbar;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        // Android 15+ 强制 edge-to-edge，官方 API 统一处理系统栏样式与对比度
        EdgeToEdge.enable(this);

        super.onCreate(savedInstanceState);
        ActionBar ab = getSupportActionBar();
        if (ab != null) {
            ab.setDisplayHomeAsUpEnabled(true);
        }
    }

    public Toolbar getActionBarToolbar() {
        if (mActionBarToolbar == null) {
            mActionBarToolbar = findViewById(R.id.toolbar_actionbar);
            if (mActionBarToolbar != null) {
                // Depending on which version of Android you are on the Toolbar or the ActionBar may be
                // active so the a11y description is set here.
                mActionBarToolbar.setNavigationContentDescription(R.string.navdrawer_description_a11y);
                setSupportActionBar(mActionBarToolbar);
            }
        }
        return mActionBarToolbar;
    }

    @Override
    public void setContentView(int layoutResID) {
        super.setContentView(layoutResID);
        getActionBarToolbar();
    }

    public void applyEdgeToEdge(View view) {
        if (view == null) {
            throw new IllegalArgumentException("view is null");
        }
        setupInsets(view);
    }

    /**
     * 官方推荐：把 insets 直接绑定到具体组件。
     * - 顶部：绑定到 Toolbar（高度同步增加，背景自然延伸进状态栏）；无 Toolbar 的页面兜底给根容器
     * - 底部：有 BottomNavigationView 时绑定到它自身（背景延伸到导航栏）；否则根容器兜底处理导航栏
     */
    public static void setupInsets(View root) {
        View toolbar = root.findViewById(R.id.toolbar_actionbar);
        boolean hasToolbar = toolbar != null;

        View bottomNav = root.findViewById(R.id.nav_bottom_view);
        boolean hasBottomNav = bottomNav != null;

        // 根容器：只处理左右 system bars 和顶部（无 Toolbar 时）；底部在有 BottomNav 时跳过
        ViewCompat.setOnApplyWindowInsetsListener(root, (v, insets) -> {
            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());

            int top = hasToolbar ? 0 : systemBars.top;
            int bottom = hasBottomNav ? 0 : systemBars.bottom;
            v.setPadding(systemBars.left, top, systemBars.right, bottom);
            return insets;
        });

        if (hasToolbar) {
            ViewCompat.setOnApplyWindowInsetsListener(toolbar, (v, insets) -> {
                int top = insets.getInsets(WindowInsetsCompat.Type.statusBars()).top;
                int cur = v.getPaddingTop();
                if (cur != top) {
                    int delta = top - cur;
                    v.setPadding(v.getPaddingLeft(), top, v.getPaddingRight(), v.getPaddingBottom());
                    // 固定高度时同步增加，避免内容被状态栏 padding 压缩
                    ViewGroup.LayoutParams lp = v.getLayoutParams();
                    if (lp != null && lp.height > 0) {
                        lp.height += delta;
                        v.setLayoutParams(lp);
                    }
                }
                return insets;
            });
        }

        if (hasBottomNav) {
            // BottomNavigationView 导航栏 inset 绑定到自身，背景延伸到系统导航栏后方
            ViewCompat.setOnApplyWindowInsetsListener(bottomNav, (v, insets) -> {
                int bottom = insets.getInsets(WindowInsetsCompat.Type.navigationBars()).bottom;
                v.setPadding(v.getPaddingLeft(), v.getPaddingTop(), v.getPaddingRight(), bottom);
                return insets;
            });
        }
    }

    private Dialog loadingDialog;
    private long dialogShowTimestamp = 0L;
    private static final long MIN_DIALOG_SHOW_TIME = 500; // minimum display duration in ms
    private Runnable pendingDismissRunnable;

    @Override
    protected void onDestroy() {
        if (pendingDismissRunnable != null && loadingDialog != null && loadingDialog.getWindow() != null) {
            loadingDialog.getWindow().getDecorView().removeCallbacks(pendingDismissRunnable);
        }
        pendingDismissRunnable = null;

        safeDismiss();
        super.onDestroy();
    }

    private void safeDismiss() {
        try {
            if (loadingDialog != null && loadingDialog.isShowing()) {
                loadingDialog.dismiss();
            }
        } catch (Exception e) {
            // 忽略异常，防止因为 Activity 已经销毁导致的崩溃
        } finally {
            loadingDialog = null;
        }
    }

    private DialogInterface.OnDismissListener listener;

    public void setOnDismissListener(DialogInterface.OnDismissListener listener) {
        this.listener = listener;
    }

    public void showLoadingDialog() {
        showLoadingDialog(true);
    }

    public void showLoadingDialog(boolean cancelable) {
        if (loadingDialog == null) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setCancelable(cancelable);
            builder.setView(R.layout.layout_dialog_progress_bar);
            loadingDialog = builder.create();
            loadingDialog.setOnDismissListener(new DialogInterface.OnDismissListener() {
                @Override
                public void onDismiss(DialogInterface iDialog) {
                    // delay dismiss
                    if (listener != null) {
                        listener.onDismiss(iDialog);
                    }

                    if (pendingDismissRunnable != null && loadingDialog != null && loadingDialog.getWindow() != null) {
                        loadingDialog.getWindow().getDecorView().removeCallbacks(pendingDismissRunnable);
                    }
                }
            });
        }

        if (!loadingDialog.isShowing()) {
            loadingDialog.show();
            dialogShowTimestamp = System.currentTimeMillis();
        }
    }

    public void dismissLoadingDialog() {
        if (loadingDialog == null || !loadingDialog.isShowing()) {
            return;
        }

        // If the activity has already started to be destroyed, close it directly without delay
        if (isFinishing() || isDestroyed()) {
            safeDismiss();
            return;
        }

        long elapsed = System.currentTimeMillis() - dialogShowTimestamp;
        if (elapsed >= MIN_DIALOG_SHOW_TIME) {
            safeDismiss();
        } else {
            //deferred destroy
            if (pendingDismissRunnable != null && loadingDialog.getWindow() != null) {
                loadingDialog.getWindow().getDecorView().removeCallbacks(pendingDismissRunnable);
            }

            pendingDismissRunnable = new Runnable() {
                @Override
                public void run() {
                    // check the activity state and dialog status again
                    if (!isFinishing() && !isDestroyed() && loadingDialog != null && loadingDialog.isShowing()) {
                        safeDismiss();
                    }
                    pendingDismissRunnable = null;
                }
            };

            if (loadingDialog != null && loadingDialog.getWindow() != null) {
                long delay = MIN_DIALOG_SHOW_TIME - elapsed;
                loadingDialog.getWindow().getDecorView().postDelayed(pendingDismissRunnable, delay);
            }
        }
    }
}
