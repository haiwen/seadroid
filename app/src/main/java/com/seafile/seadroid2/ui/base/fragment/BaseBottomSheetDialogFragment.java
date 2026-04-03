package com.seafile.seadroid2.ui.base.fragment;

import android.widget.FrameLayout;

import androidx.coordinatorlayout.widget.CoordinatorLayout;

import com.blankj.utilcode.util.BarUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.R;

public abstract class BaseBottomSheetDialogFragment extends BottomSheetDialogFragment {

    public BottomSheetBehavior<FrameLayout> behavior;

//    @NonNull
//    @Override
//    public Dialog onCreateDialog(Bundle savedInstanceState) {
//        BottomSheetDialog bottomSheetDialog = new BottomSheetDialog(requireContext());
//        bottomSheetDialog.setCancelable(true);
//        bottomSheetDialog.setCanceledOnTouchOutside(true);
//        bottomSheetDialog.setDismissWithAnimation(true);
//        return bottomSheetDialog;
//    }
////
    @Override
    public void onStart() {
        super.onStart();
//        ((View) getLayoutRootView().getParent()).setBackgroundResource(R.color.transparent);

        BottomSheetDialog dialog = (BottomSheetDialog) getDialog();
        FrameLayout bottomSheet = dialog.getDelegate().findViewById(R.id.design_bottom_sheet);

        if (bottomSheet != null) {
            CoordinatorLayout.LayoutParams layoutParams = (CoordinatorLayout.LayoutParams) bottomSheet.getLayoutParams();
            layoutParams.height = getHeight();
            behavior = BottomSheetBehavior.from(bottomSheet);
            behavior.setState(BottomSheetBehavior.STATE_HALF_EXPANDED);
        }
        //        View outSide = dialog.getDelegate().findViewById(com.google.android.material.R.id.touch_outside);
//        outSide.setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                ToastUtils.showLong("out side");
//            }
//        });
    }
////
//
    /**
     * 获取屏幕高度
     *
     * @return height
     */
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

//    protected abstract View getLayoutRootView();
}
