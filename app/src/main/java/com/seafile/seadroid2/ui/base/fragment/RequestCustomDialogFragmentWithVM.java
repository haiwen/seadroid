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

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;

import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;

public abstract class RequestCustomDialogFragmentWithVM<VM extends BaseViewModel> extends BaseDialogFragmentWithVM<VM> {
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

    public void refreshData(boolean status) {
        if (mListener != null) {
            mListener.onActionStatus(status);
        }
    }

    protected abstract int getLayoutId();

    protected abstract void onPositiveClick();

    protected void onNegativeClicked() {

    }

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

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        return super.onCreateView(inflater, container, savedInstanceState);
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
            negativeView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onNegativeClicked();
                    dismiss();
                }
            });
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
        dialog.setCanceledOnTouchOutside(false);
        dialog.setCancelable(false);

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


    /////////////////////////////////////////TextInput Error///////////////////////////////////////////
    private int textInputLayoutId;

    private final int countDownDuration = 2;
    private Disposable disposable;

    public void setInputError(@IdRes int textInputLayoutId, String error) {
        this.textInputLayoutId = textInputLayoutId;

        TextInputLayout passwordInputLayout = getDialogView().findViewById(textInputLayoutId);
        passwordInputLayout.setError(error);

        if (!TextUtils.isEmpty(error)) {
            restoreTextInputError();
        }
    }

    private void restoreTextInputError() {
        if (disposable != null && !disposable.isDisposed()) {
            disposable.dispose();
        }

        Observable.interval(1, TimeUnit.SECONDS)
                .take(countDownDuration + 1)
                .map(new Function<Long, Long>() {
                    @Override
                    public Long apply(Long aLong) throws Exception {
                        return countDownDuration - aLong;
                    }
                })
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(observer);

        getViewModel().addDisposable(disposable);
    }


    private final io.reactivex.Observer<Long> observer = new io.reactivex.Observer<Long>() {
        @Override
        public void onSubscribe(Disposable d) {
            disposable = d;
        }

        @Override
        public void onNext(Long aLong) {
            SLogs.d("Timer：" + aLong);
        }

        @Override
        public void onError(Throwable e) {

        }

        @Override
        public void onComplete() {
            SLogs.d("Timer：finish");

            if (textInputLayoutId != 0) {
                setInputError(textInputLayoutId, null);
            }
        }
    };


}
