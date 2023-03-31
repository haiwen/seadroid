package com.seafile.seadroid2.ui.bottomsheet;

import android.app.Dialog;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.design.widget.BottomSheetBehavior;
import android.support.design.widget.BottomSheetDialog;
import android.support.design.widget.BottomSheetDialogFragment;
import android.support.design.widget.CoordinatorLayout;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.seafile.seadroid2.R;

public abstract class BaseBottomSheetDialogFragment extends BottomSheetDialogFragment {

    private View mRootView;

    public BottomSheetBehavior<FrameLayout> behavior;

    public View getRootView() {
        return mRootView;
    }

    public void e(String e) {
        Log.e(this.getClass().getSimpleName(), " => " + e);
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        return new BottomSheetDialog(requireContext(), R.style.BottomSheetStyle);
    }

    @NonNull
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mRootView = inflater.inflate(getLayoutId(), container, false);
        initView();
        if (getCancelId() != 0) {
            View cancelView = mRootView.findViewById(getCancelId());
            cancelView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    dismiss();
                }
            });
        }
        return mRootView;
    }

    @Override
    public void onViewCreated(@NonNull View view, Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        init();
    }

    @Override
    public void onStart() {
        super.onStart();
        ((View) getRootView().getParent()).setBackgroundResource(R.color.transparent);

        BottomSheetDialog dialog = (BottomSheetDialog) getDialog();
        FrameLayout bottomSheet = dialog.getDelegate().findViewById(android.support.design.R.id.design_bottom_sheet);

        if (bottomSheet != null) {
            CoordinatorLayout.LayoutParams layoutParams = (CoordinatorLayout.LayoutParams) bottomSheet.getLayoutParams();
            layoutParams.height = getHeight();
            behavior = BottomSheetBehavior.from(bottomSheet);
            behavior.setState(BottomSheetBehavior.STATE_COLLAPSED);
        }
    }

    private int getHeight() {

        int height = ScreenUtils.getScreenHeight();
        if (BarUtils.isNavBarVisible(getActivity().getWindow())) {
            height -= BarUtils.getNavBarHeight();
        }
        return height;
    }

    public BottomSheetBehavior<FrameLayout> getBehavior() {
        return behavior;
    }

    protected abstract int getLayoutId();

    protected abstract int getCancelId();

    protected abstract void initView();

    protected abstract void init();
}
