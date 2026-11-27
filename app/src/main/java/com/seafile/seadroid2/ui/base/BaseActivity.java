package com.seafile.seadroid2.ui.base;

import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.SystemClock;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowInsets;

import androidx.activity.EdgeToEdge;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.core.app.ComponentActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.webkit.internal.ApiFeature;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

/**
 * A base activity that handles common functionality in the app. This includes Action Bar tweaks.
 */
public class BaseActivity extends AppCompatActivity {

    private Toolbar mToolbar;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        // Android 15+ enforces edge-to-edge; the official API handles system bar style and contrast uniformly
        EdgeToEdge.enable(this);

        super.onCreate(savedInstanceState);
    }

    public void initToolbar() {
        mToolbar = findViewById(R.id.toolbar_actionbar);
        if (mToolbar == null) {
            throw new RuntimeException("not found toolbar(R.id.toolbar_actionbar), please check your layout.");
        }

        // Depending on which version of Android you are on the Toolbar or the ActionBar may be
        // active so the a11y description is set here.
        mToolbar.setNavigationContentDescription(R.string.navdrawer_description_a11y);
        setSupportActionBar(mToolbar);
    }

    @NonNull
    public Toolbar getActionBarToolbar() {
        if (mToolbar == null) {
            throw new RuntimeException("Toolbar is null, please first call initToolbar() after setContentView().");
        }
        return mToolbar;
    }


    public void applyEdgeToEdge(View view) {
        if (view == null) {
            throw new IllegalArgumentException("view is null");
        }
        setupInsets(view);
    }

    /**
     * Official recommendation: bind insets directly to concrete components.
     * - Top: bind to the Toolbar (height grows accordingly, the background naturally extends into the status bar); pages without a Toolbar fall back to the root container
     * - Bottom: when there is a BottomNavigationView, bind to it (the background extends into the navigation bar); otherwise the root container handles the navigation bar as a fallback
     */
    public static void setupInsets(View root) {
        View toolbar = root.findViewById(R.id.toolbar_actionbar);
        boolean hasToolbar = toolbar != null;

        View bottomNav = root.findViewById(R.id.nav_bottom_view);
        boolean hasBottomNav = bottomNav != null;

        final int rootPaddingLeft = root.getPaddingLeft();
        final int rootPaddingTop = root.getPaddingTop();
        final int rootPaddingRight = root.getPaddingRight();
        final int rootPaddingBottom = root.getPaddingBottom();

        // Root container: only handles the left/right system bars and the top (when there is no Toolbar); the bottom is skipped when a BottomNav exists
        ViewCompat.setOnApplyWindowInsetsListener(root, (v, insets) -> {
            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());

            boolean bottomNavVisible = hasBottomNav && bottomNav.getVisibility() == View.VISIBLE;
            int top = hasToolbar ? rootPaddingTop : rootPaddingTop + systemBars.top;
            int bottom = bottomNavVisible
                    ? rootPaddingBottom : rootPaddingBottom + systemBars.bottom;
            v.setPadding(rootPaddingLeft + systemBars.left, top,
                    rootPaddingRight + systemBars.right, bottom);
            return insets;
        });

        if (hasToolbar) {
            final int toolbarPaddingLeft = toolbar.getPaddingLeft();
            final int toolbarPaddingTop = toolbar.getPaddingTop();
            final int toolbarPaddingRight = toolbar.getPaddingRight();
            final int toolbarPaddingBottom = toolbar.getPaddingBottom();
            final int toolbarInitialHeight = toolbar.getLayoutParams() == null
                    ? ViewGroup.LayoutParams.WRAP_CONTENT : toolbar.getLayoutParams().height;
            ViewCompat.setOnApplyWindowInsetsListener(toolbar, (v, insets) -> {
                int top = insets.getInsets(WindowInsetsCompat.Type.statusBars()).top;
                v.setPadding(toolbarPaddingLeft,
                        toolbarPaddingTop + top,
                        toolbarPaddingRight,
                        toolbarPaddingBottom);

                // The status-bar inset is part of the edge-to-edge toolbar area. Keep the
                // original action-bar height and recompute from it on every callback so the
                // action icons are not compressed or cumulatively enlarged.
                if (toolbarInitialHeight > 0) {
                    ViewGroup.LayoutParams layoutParams = v.getLayoutParams();
                    int targetHeight = toolbarInitialHeight + top;
                    if (layoutParams != null && layoutParams.height != targetHeight) {
                        layoutParams.height = targetHeight;
                        v.setLayoutParams(layoutParams);
                    }
                }
                return insets;
            });
        }

        if (hasBottomNav) {
            final int bottomNavPaddingLeft = bottomNav.getPaddingLeft();
            final int bottomNavPaddingTop = bottomNav.getPaddingTop();
            final int bottomNavPaddingRight = bottomNav.getPaddingRight();
            final int bottomNavPaddingBottom = bottomNav.getPaddingBottom();
            // Bind the BottomNavigationView navigation bar inset to itself so the background extends behind the system navigation bar
            ViewCompat.setOnApplyWindowInsetsListener(bottomNav, (v, insets) -> {
                int bottom = insets.getInsets(WindowInsetsCompat.Type.navigationBars()).bottom;
                v.setPadding(bottomNavPaddingLeft,
                        bottomNavPaddingTop,
                        bottomNavPaddingRight,
                        bottomNavPaddingBottom + bottom);
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
            // Ignore the exception to avoid a crash caused by the Activity already being destroyed
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
            dialogShowTimestamp = SystemClock.elapsedRealtime();
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

        long elapsed = SystemClock.elapsedRealtime() - dialogShowTimestamp;
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
