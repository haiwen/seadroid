package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.ClearCacheViewModel;

import io.reactivex.functions.Consumer;

public class ClearCacheDialogFragment extends RequestCustomDialogFragmentWithVM<ClearCacheViewModel> {
    public static ClearCacheDialogFragment newInstance() {

        Bundle args = new Bundle();

        ClearCacheDialogFragment fragment = new ClearCacheDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    protected void onPositiveClick() {
        getViewModel().clear(new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {
                //
                refreshData(aBoolean);

                dismiss();
            }
        });
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.settings_clear_cache_title;
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.settings_clear_cache_hint);
    }
}
