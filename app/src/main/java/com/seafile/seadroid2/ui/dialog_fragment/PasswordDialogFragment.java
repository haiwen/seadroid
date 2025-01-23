package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.widget.EditText;
import android.widget.LinearLayout;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.data.model.TResultModel;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.PasswordViewModel;

public class PasswordDialogFragment extends RequestCustomDialogFragmentWithVM<PasswordViewModel> {
    private String repoId;
    private String repoName;

    private OnResultListener<RepoModel> resultListener;

    public void setResultListener(OnResultListener<RepoModel> resultListener) {
        this.resultListener = resultListener;
    }

    public static PasswordDialogFragment newInstance(String repoId, String repoName) {
        Bundle args = new Bundle();
        args.putString("repoId", repoId);
        args.putString("repoName", repoName);
        PasswordDialogFragment fragment = new PasswordDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bundle args = getArguments();
        if (args != null) {
            repoId = args.getString("repoId");
            repoName = args.getString("repoName");
        } else {
            throw new IllegalArgumentException("this dialogFragment need repoId param");
        }
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
    protected void onNegativeClicked() {

        if (resultListener != null) {
            resultListener.onResultData(null);
        }

        super.onNegativeClicked();
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
        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException seafException) {
                setInputError(R.id.password_hint, seafException.getMessage());
            }
        });
        getViewModel().getActionResultLiveData().observe(this, new Observer<TResultModel<RepoModel>>() {
            @Override
            public void onChanged(TResultModel<RepoModel> tResultModel) {
                if (tResultModel.success) {

                    if (resultListener != null) {
                        resultListener.onResultData(tResultModel.data);
                    }

                    dismiss();
                } else if (!TextUtils.isEmpty(tResultModel.error_msg)) {
                    setInputError(R.id.password_hint, tResultModel.error_msg);
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
