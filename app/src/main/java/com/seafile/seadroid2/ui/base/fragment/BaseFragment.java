package com.seafile.seadroid2.ui.base.fragment;

import android.app.Dialog;

import androidx.fragment.app.Fragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

public class BaseFragment extends Fragment {
    private boolean isFirstLoadData = true;

    @Override
    public void onResume() {
        super.onResume();
        if (isFirstLoadData) {
            isFirstLoadData = false;
            onFirstResume();
        } else {
            onOtherResume();
        }
    }

    public void onFirstResume() {

    }

    public void onOtherResume() {

    }

    private Dialog dialog;

    public void showLoadingDialog(boolean isShow) {
        if (isShow) {
            showLoadingDialog();
        } else {
            dismissLoadingDialog();
        }
    }

    public void showLoadingDialog() {
        if (dialog == null) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
            builder.setView(R.layout.layout_dialog_progress_bar);
            dialog = builder.create();
        }

        if (!dialog.isShowing()) {
            dialog.show();
        }
    }

    public void dismissLoadingDialog() {
        if (dialog != null && dialog.isShowing()) {
            dialog.dismiss();
        }
    }
}
