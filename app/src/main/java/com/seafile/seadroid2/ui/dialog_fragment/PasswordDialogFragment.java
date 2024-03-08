package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.widget.EditText;
import android.widget.LinearLayout;

import androidx.lifecycle.Observer;

import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.crypto.Crypto;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.data.model.ResultModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.PasswordViewModel;

import io.reactivex.functions.Consumer;

public class PasswordDialogFragment extends RequestCustomDialogFragmentWithVM<PasswordViewModel> {
    private String repoId;
    private String repoName;

    public static PasswordDialogFragment newInstance() {

        Bundle args = new Bundle();

        PasswordDialogFragment fragment = new PasswordDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    public void initData(String repoId, String repoName) {
        this.repoId = repoId;
        this.repoName = repoName;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_password;
    }

    @Override
    public String getDialogTitleString() {
        return repoName;
    }

    @Override
    protected void onPositiveClick() {
        if (!checkData()) {
            return;
        }

        EditText editText = getDialogView().findViewById(R.id.password);
        String password = editText.getText().toString();

        //verify password
        getViewModel().verifyPwd(repoId, password);
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        if (TextUtils.isEmpty(repoId)) {
            throw new IllegalArgumentException("this dialogFragment need repoId param");
        }

        EditText editText = getDialogView().findViewById(R.id.password);
        TextInputLayout passwordInputLayout = getDialogView().findViewById(R.id.password_hint);

        passwordInputLayout.setEndIconOnClickListener(v -> {
            if (editText.getTransformationMethod() instanceof PasswordTransformationMethod) {
                editText.setTransformationMethod(HideReturnsTransformationMethod.getInstance());
                passwordInputLayout.setEndIconDrawable(R.drawable.icon_eye_open);
            } else {
                editText.setTransformationMethod(PasswordTransformationMethod.getInstance());
                passwordInputLayout.setEndIconDrawable(R.drawable.icon_eye_close);
            }

            String input = editText.getText().toString().trim();
            if (!TextUtils.isEmpty(input)) {
                editText.setSelection(input.length());
            }
        });
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();
        getViewModel().getActionLiveData().observe(this, new Observer<ResultModel>() {
            @Override
            public void onChanged(ResultModel resultModel) {
                if (resultModel.success) {
                    refreshData();
                    dismiss();
                } else if (!TextUtils.isEmpty(resultModel.error_msg)) {
                    setInputError(R.id.password_hint, resultModel.error_msg);
                }
            }
        });
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoading(aBoolean);
            }
        });
    }


    private boolean checkData() {
        EditText editText = getDialogView().findViewById(R.id.password);
        Editable editable = editText.getText();
        if (editable == null || editable.length() == 0 || TextUtils.isEmpty(editable.toString().trim())) {
            setInputError(R.id.password_hint, getString(R.string.password_empty));
            return false;
        }

        if (editable.length() < Constants.PASSWORD_MINIMUM_LENGTH) {
            setInputError(R.id.password_hint, getString(R.string.err_passwd_too_short));
            return false;
        }

        return true;
    }


}
