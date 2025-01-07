package com.seafile.seadroid2.ui.base;

import android.app.Dialog;
import android.os.Bundle;

import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

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
        super.onCreate(savedInstanceState);
        ActionBar ab = getSupportActionBar();
        if (ab != null) {
            ab.setDisplayHomeAsUpEnabled(true);
        }
    }

    protected Toolbar getActionBarToolbar() {
        if (mActionBarToolbar == null) {
            mActionBarToolbar = (Toolbar) findViewById(R.id.toolbar_actionbar);
            if (mActionBarToolbar != null) {
                // Depending on which version of Android you are on the Toolbar or the ActionBar may be
                // active so the a11y description is set here.
                mActionBarToolbar.setNavigationContentDescription(getResources().getString(R.string
                        .navdrawer_description_a11y));
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

    private Dialog loadingDialog;

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
            builder.setView(R.layout.layout_dialog_progress_bar);
            loadingDialog = builder.create();
        }

        if (!loadingDialog.isShowing()) {
            loadingDialog.show();
        }
    }

    public void dismissLoadingDialog() {
        if (loadingDialog != null && loadingDialog.isShowing()) {
            loadingDialog.dismiss();
        }
    }
}
