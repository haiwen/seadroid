package com.seafile.seadroid2.ui.base.fragment;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.util.TUtil;

public class BaseDialogFragmentWithVM<VM extends BaseViewModel> extends BaseDialogFragment {
    private VM tvm;

    public VM getViewModel() {
        return tvm;
    }


    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        initTvm();
    }

    private void initTvm() {
        VM t = TUtil.getT(this, 0);
        if (t == null) {
            throw new IllegalStateException("VM generic parameters that inherit BaseViewModel cannot be instantiated");
        }

        ViewModel viewModel = new ViewModelProvider(this).get(t.getClass());
        tvm = (VM) viewModel;
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();

        if (tvm != null) {
            tvm.disposeAll();
            tvm = null;
        }
    }
}
