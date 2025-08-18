package com.seafile.seadroid2.ui.base;

import android.os.Bundle;

import androidx.lifecycle.ViewModelProvider;

import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.lang.reflect.ParameterizedType;

public class BaseActivityWithVM<VM extends BaseViewModel> extends BaseActivity {
    private VM tvm;

    public VM getViewModel() {
        if (null == tvm) {
            initTvm();
        }
        return tvm;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (tvm != null) {
            tvm.clearAll();
            tvm = null;
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
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
