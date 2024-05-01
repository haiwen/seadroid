package com.seafile.seadroid2.ui.base;

import android.os.Bundle;

import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;
import com.seafile.seadroid2.framework.util.TUtil;

public class BaseActivityWithVM<V extends BaseViewModel> extends BaseActivity {
    private V tvm;

    public V getViewModel() {
        return tvm;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        initViewModelClass();
    }

    protected void initViewModelClass() {
        V t = TUtil.getT(this, 0);
        if (t == null) {
            throw new IllegalStateException("VM generic parameters that inherit BaseViewModel cannot be instantiated");
        }

        ViewModel viewModel = new ViewModelProvider(this).get(t.getClass());
        tvm = (V) viewModel;
    }
}
