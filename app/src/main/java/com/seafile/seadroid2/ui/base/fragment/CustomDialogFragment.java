package com.seafile.seadroid2.ui.base.fragment;

import android.app.Dialog;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.IdRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.fragment.app.DialogFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.util.SLogs;

import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;

public abstract class CustomDialogFragment extends DialogFragment {
    // Use this instance of the interface to deliver action events
    private OnRefreshDataListener mListener;

    public OnRefreshDataListener getRefreshListener() {
        return mListener;
    }

    public void setRefreshListener(OnRefreshDataListener mListener) {
        this.mListener = mListener;
    }

    public void refreshData() {
        if (mListener != null) {
            mListener.onActionStatus(true);
        }
    }

    protected abstract int getLayoutId();

    protected abstract void onPositiveClick();

    private View rootView;

    public View getDialogView() {
        return rootView;
    }

    private TextView positiveView;
    private TextView negativeView;
    private ProgressBar loadingBar;

    public TextView getPositiveView() {
        return positiveView;
    }

    public TextView getNegativeView() {
        return negativeView;
    }

    public ProgressBar getLoadingBar() {
        return loadingBar;
    }

    public void showLoading(boolean isShow) {
        if (positiveView != null) {
            positiveView.setVisibility(isShow ? View.GONE : View.VISIBLE);
        }
        if (negativeView != null) {
            negativeView.setVisibility(isShow ? View.GONE : View.VISIBLE);
        }
        if (loadingBar != null) {
            loadingBar.setVisibility(isShow ? View.VISIBLE : View.GONE);
        }
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        rootView = LayoutInflater.from(requireContext()).inflate(R.layout.layout_dialog_container, null);
        LinearLayout containerView = rootView.findViewById(R.id.container);

        //
        LayoutInflater.from(requireContext()).inflate(getLayoutId(), containerView);

        //action bar
        positiveView = rootView.findViewById(R.id.text_view_positive);
        negativeView = rootView.findViewById(R.id.text_view_negative);
        loadingBar = rootView.findViewById(R.id.progress_bar);

        if (positiveView != null) {
            positiveView.setOnClickListener(v -> onPositiveClick());
        }

        if (negativeView != null) {
            negativeView.setOnClickListener(v -> dismiss());
        }

        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireActivity());

        //title
        String dialogTitleString = getDialogTitleString();
        int dialogTitleRes = getDialogTitleRes();

        if (dialogTitleRes != 0) {
            builder.setTitle(dialogTitleRes);
        } else if (!TextUtils.isEmpty(dialogTitleString)) {
            builder.setTitle(dialogTitleString);
        }

        //
        initView(containerView);

        //
        builder.setView(getDialogView());

        //
        initViewModel();

        final AlertDialog dialog = builder.create();

        onDialogCreated(dialog);

        return dialog;
    }

    protected void initView(LinearLayout containerView) {
    }

    protected void initViewModel() {
    }

    /**
     * This hook method is called right after the dialog is built.
     */
    protected void onDialogCreated(Dialog dialog) {
    }

    public int getDialogTitleRes() {
        return 0;
    }

    public String getDialogTitleString() {
        return "";
    }
}
