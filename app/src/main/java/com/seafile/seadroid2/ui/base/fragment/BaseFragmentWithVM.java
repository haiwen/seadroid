package com.seafile.seadroid2.ui.base.fragment;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.chad.library.adapter4.QuickAdapterHelper;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.lang.reflect.ParameterizedType;

public class BaseFragmentWithVM<VM extends BaseViewModel> extends BaseFragment {
    private VM tvm;

    public VM getViewModel() {
        if (null == tvm) {
            initTvm();
        }
        return tvm;
    }

    private QuickAdapterHelper helper;

    public QuickAdapterHelper createAdapterHelper(BaseAdapter<?, ?> adapter) {
        if (null == helper) {
            helper = new QuickAdapterHelper.Builder(adapter).build();
        }
        return helper;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (tvm != null) {
            tvm.disposeAll();
            tvm = null;
        }
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        initTvm();
    }


    private void initTvm() {
        tvm = new ViewModelProvider(this).get(getViewModelClass());
    }

    @SuppressWarnings("unchecked")
    private Class<VM> getViewModelClass() {
        ParameterizedType type = (ParameterizedType) getClass().getGenericSuperclass();
        return (Class<VM>) type.getActualTypeArguments()[0];
    }
}
