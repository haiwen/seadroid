package com.seafile.seadroid2.ui.base;

import android.app.Dialog;
import android.content.DialogInterface;
import android.graphics.Color;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.LinearLayout;

import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowCompat;
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
        // support for Android 15
        WindowCompat.setDecorFitsSystemWindows(getWindow(), false);
//
        getWindow().setStatusBarColor(Color.TRANSPARENT);
        getWindow().setNavigationBarColor(Color.TRANSPARENT);

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

        View statusBarGuideline = view.findViewById(R.id.status_bar_guideline);
        if (statusBarGuideline == null) {
            throw new IllegalArgumentException("view must not contain a view with id 'status_bar_guideline'");
        }

        ViewCompat.setOnApplyWindowInsetsListener(view, (v, insets) -> {
            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(
                    systemBars.left,
                    0,
                    systemBars.right,
                    systemBars.bottom
            );


            Insets statusBars = insets.getInsets(WindowInsetsCompat.Type.statusBars());
            ViewGroup.LayoutParams lp = statusBarGuideline.getLayoutParams();
            lp.height = statusBars.top;
            statusBarGuideline.setLayoutParams(lp);
            return insets;
        });
    }

    private Dialog loadingDialog;
    private long dialogShowTimestamp = 0L;
    private static final long MIN_DIALOG_SHOW_TIME = 500; // minimum display duration in ms
    private Runnable pendingDismissRunnable;

    @Override
    protected void onDestroy() {
        dismissLoadingDialog();
        super.onDestroy();
    }

    public boolean isDialogShowing() {
        if (loadingDialog == null) {
            return false;
        }

        return loadingDialog.isShowing();
    }

    public void showLoadingDialog(boolean isShow) {
        if (isShow) {
            showLoadingDialog();
        } else {
            dismissLoadingDialog();
        }
    }

    public void showLoadingDialog() {
        if (loadingDialog == null) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setCancelable(false);
            builder.setView(R.layout.layout_dialog_progress_bar);
            builder.setCancelable(false);
            loadingDialog = builder.create();
            loadingDialog.setOnDismissListener(new DialogInterface.OnDismissListener() {
                @Override
                public void onDismiss(DialogInterface iDialog) {
                    // delay dismiss
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
        long elapsed = System.currentTimeMillis() - dialogShowTimestamp;
        if (elapsed >= MIN_DIALOG_SHOW_TIME) {
            loadingDialog.dismiss();
        } else {
            // delay dismiss
            if (pendingDismissRunnable != null && loadingDialog.getWindow() != null) {
                loadingDialog.getWindow().getDecorView().removeCallbacks(pendingDismissRunnable);
            }

            pendingDismissRunnable = new Runnable() {
                @Override
                public void run() {
                    if (loadingDialog != null && loadingDialog.isShowing()) {
                        loadingDialog.dismiss();
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
