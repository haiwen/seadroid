package com.seafile.seadroid2.ui.base.fragment;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetDialogFragment;
import com.seafile.seadroid2.ui.base.viewmodel.BaseViewModel;

import java.lang.reflect.ParameterizedType;

public class BaseBottomSheetDialogFragmentWithVM<VM extends BaseViewModel> extends BottomSheetDialogFragment {
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
        tvm = new ViewModelProvider(this).get(getViewModelClass());
    }

    @SuppressWarnings("unchecked")
    private Class<VM> getViewModelClass() {
        ParameterizedType type = (ParameterizedType) getClass().getGenericSuperclass();
        return (Class<VM>) type.getActualTypeArguments()[0];
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
