package com.seafile.seadroid2.ui.base.fragment;

import android.app.Dialog;
import android.content.DialogInterface;

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
    private long dialogShowTimestamp = 0L;
    private static final long MIN_DIALOG_SHOW_TIME = 500; // minimum display duration in ms
    private Runnable pendingDismissRunnable;

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
            builder.setCancelable(false);
            dialog = builder.create();
            dialog.setOnDismissListener(new DialogInterface.OnDismissListener() {
                @Override
                public void onDismiss(DialogInterface iDialog) {
                    // delay dismiss
                    if (pendingDismissRunnable != null && dialog != null && dialog.getWindow() != null) {
                        dialog.getWindow().getDecorView().removeCallbacks(pendingDismissRunnable);
                    }
                }
            });
        }
        if (!dialog.isShowing()) {
            dialog.show();
            dialogShowTimestamp = System.currentTimeMillis();
        }
    }

    public void dismissLoadingDialog() {
        if (dialog == null || !dialog.isShowing()) {
            return;
        }

        long elapsed = System.currentTimeMillis() - dialogShowTimestamp;
        if (elapsed >= MIN_DIALOG_SHOW_TIME) {
            dialog.dismiss();
        } else {
            // delay dismiss
            if (pendingDismissRunnable != null && dialog.getWindow() != null) {
                dialog.getWindow().getDecorView().removeCallbacks(pendingDismissRunnable);
            }

            pendingDismissRunnable = new Runnable() {
                @Override
                public void run() {
                    if (dialog != null && dialog.isShowing()) {
                        dialog.dismiss();
                    }

                    pendingDismissRunnable = null;
                }
            };

            if (dialog != null && dialog.getWindow() != null) {
                long delay = MIN_DIALOG_SHOW_TIME - elapsed;
                dialog.getWindow().getDecorView().postDelayed(pendingDismissRunnable, delay);
            }
        }
    }
}
