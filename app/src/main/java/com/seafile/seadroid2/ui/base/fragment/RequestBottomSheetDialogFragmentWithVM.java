package com.seafile.seadroid2.ui.base.fragment;

import android.app.Dialog;
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.IdRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsAnimationCompat;
import androidx.core.view.WindowInsetsCompat;

import com.blankj.utilcode.util.KeyboardUtils;
import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.button.MaterialButton;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;

import java.util.List;
import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Function;

public abstract class RequestBottomSheetDialogFragmentWithVM<VM extends BaseViewModel> extends BaseBottomSheetDialogFragmentWithVM<VM> {
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

    protected abstract String getTitle();

    protected abstract void onPositiveClick();

    protected void onNegativeClicked() {
        dismissDialogWithIme();
    }

    private View rootView;
    private LinearLayout childContainerView;
    private MaterialButton positiveView;
    private MaterialButton negativeView;
    private ProgressBar loadingBar;

    public View getDialogView() {
        return rootView;
    }

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

    // first call
    @NonNull
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        BottomSheetDialog bottomSheetDialog = new BottomSheetDialog(requireContext());
        bottomSheetDialog.setCancelable(true);
        bottomSheetDialog.setCanceledOnTouchOutside(true);
        bottomSheetDialog.setDismissWithAnimation(true);
        return bottomSheetDialog;
    }


    //second call
    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {

        rootView = LayoutInflater.from(requireContext()).inflate(R.layout.dialog_bottom_sheet_container, null);
        childContainerView = rootView.findViewById(R.id.child_container);

        LayoutInflater.from(requireContext()).inflate(getLayoutId(), childContainerView);

        //action bar
        TextView titleView = rootView.findViewById(R.id.title);
        if (titleView != null) {
            if (TextUtils.isEmpty(getTitle())) {
                titleView.setVisibility(View.GONE);
            } else {
                titleView.setText(getTitle());
            }
        }

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
                }
            });
        }

        initView(childContainerView);
        initViewModel();

        return rootView;
    }


    //third call
    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initDialog();
        initRootView();
    }

    private BottomSheetBehavior<FrameLayout> bottomSheetBehavior;

    private void initDialog() {
//        if (getDialog() != null) {
//            Window window = getDialog().getWindow();
//            if (window != null) {
//                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
//                    window.setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE | WindowManager.LayoutParams.SOFT_INPUT_ADJUST_NOTHING);
//                } else {
//                    window.setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
//                }
//            }
//        }

        BottomSheetDialog bottomSheetDialog = (BottomSheetDialog) getDialog();
        if (bottomSheetDialog != null) {
            bottomSheetBehavior = bottomSheetDialog.getBehavior();
            bottomSheetBehavior.setState(BottomSheetBehavior.STATE_EXPANDED);
//            bottomSheetBehavior.addBottomSheetCallback(bottomSheetCallback);
        }
    }


    private final BottomSheetBehavior.BottomSheetCallback bottomSheetCallback = new BottomSheetBehavior.BottomSheetCallback() {
        @Override
        public void onStateChanged(@NonNull View bottomSheet, int newState) {
            if (newState == BottomSheetBehavior.STATE_DRAGGING) {
//                bottomSheetBehavior.setState(BottomSheetBehavior.STATE_EXPANDED);
            }
        }

        @Override
        public void onSlide(@NonNull View bottomSheet, float slideOffset) {

        }
    };
//
//    private boolean canDismiss = false;
//    private boolean isDismissing = false;

    public void dismissDialogWithIme() {
        KeyboardUtils.hideSoftInput(getDialog().getWindow());
        dismiss();
    }

    private void initRootView() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            ViewCompat.setWindowInsetsAnimationCallback(rootView.getRootView(), new WindowInsetsAnimationCompat.Callback(WindowInsetsAnimationCompat.Callback.DISPATCH_MODE_STOP) {

//                        private boolean lastImeVisible = false;
                        private int startHeight = 0;
                        private int lastDiffH = 0;

                        @Override
                        public void onPrepare(@NonNull WindowInsetsAnimationCompat animation) {
                            if (startHeight == 0) {
                                startHeight = rootView.getHeight();
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

                            ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams) rootView.getLayoutParams();
                            layoutParams.bottomMargin = diffH;
                            rootView.setLayoutParams(layoutParams);

//                            boolean isIme = insets.isVisible(WindowInsetsCompat.Type.ime());
//                            if (lastImeVisible == isIme) {
//
//                            } else {
//                                lastImeVisible = isIme;
//                                if (!lastImeVisible && canDismiss && !isDismissing) {
//                                    isDismissing = true;
//                                    dismiss();
//                                    ViewCompat.setWindowInsetsAnimationCallback(rootView, null);
//                                }
//                            }

//                            SLogs.e("isIme: " + isIme);
                            // keyboard hide and dismiss
//                            if (diffH < lastDiffH && canDismiss) {
//                                dismiss();
//                                ViewCompat.setWindowInsetsAnimationCallback(rootView, null);
//                            }

                            lastDiffH = diffH;
                            return insets;
                        }
                    }
            );
        } else {
            // <= Android R
            rootView.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
                int lastBottom = 0;

                @Override
                public void onGlobalLayout() {
                    WindowInsetsCompat insets = ViewCompat.getRootWindowInsets(rootView);
                    if (insets != null) {
                        int bottom = insets.getInsets(WindowInsetsCompat.Type.ime()).bottom;
                        if (lastBottom != 0 && bottom == 0) {
                            dismiss();
                            rootView.getViewTreeObserver().removeOnGlobalLayoutListener(this);
                        }
                        lastBottom = bottom;
                    }
                }
            });
        }
    }

    protected void initView(LinearLayout containerView) {
    }

    protected void initViewModel() {
    }
}
