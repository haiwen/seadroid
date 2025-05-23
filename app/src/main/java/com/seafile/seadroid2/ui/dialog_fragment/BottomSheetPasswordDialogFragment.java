package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.view.View;
import android.widget.EditText;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.KeyboardUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.TResultModel;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnResultListener;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.PasswordViewModel;

public class BottomSheetPasswordDialogFragment extends RequestBottomSheetDialogFragmentWithVM<PasswordViewModel> {

    private Account account;
    private String repoName, repoId;
    private OnResultListener<RepoModel> resultListener;

    public void setResultListener(OnResultListener<RepoModel> resultListener) {
        this.resultListener = resultListener;
    }

    public static BottomSheetPasswordDialogFragment newInstance(String repoId, String repoName) {
        Bundle args = new Bundle();
        args.putString("repoId", repoId);
        args.putString("repoName", repoName);
        BottomSheetPasswordDialogFragment fragment = new BottomSheetPasswordDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    public static BottomSheetPasswordDialogFragment newInstance(Account account, String repoId, String repoName) {
        Bundle args = new Bundle();
        args.putString("repoId", repoId);
        args.putString("repoName", repoName);
        args.putParcelable("account", account);
        BottomSheetPasswordDialogFragment fragment = new BottomSheetPasswordDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle args = getArguments();
        if (args != null) {
            account = args.getParcelable("account");
            repoId = args.getString("repoId");
            repoName = args.getString("repoName");

            if (account == null) {
                account = SupportAccountManager.getInstance().getCurrentAccount();
            }

            if (TextUtils.isEmpty(repoId)) {
                throw new IllegalArgumentException("this dialogFragment need repoId param");
            }

            if (TextUtils.isEmpty(repoName)) {
                throw new IllegalArgumentException("this dialogFragment need repoName param");
            }
        } else {
            throw new IllegalArgumentException("this dialogFragment need params");
        }
    }


    @Override
    protected int getLayoutId() {
        return R.layout.dialog_password_input;
    }

    @Override
    protected String getTitle() {
        return repoName;
    }

    @Override
    protected void initView(LinearLayout parentView) {
        super.initView(parentView);

        TextInputEditText editText = getDialogView().findViewById(R.id.password);

        TextInputLayout passwordInputLayout = getDialogView().findViewById(R.id.password_layout);
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
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        TextInputEditText editText = getDialogView().findViewById(R.id.password);
        if (editText == null) {
            return;
        }
        editText.postDelayed(new Runnable() {
            @Override
            public void run() {
                editText.requestFocus();
                KeyboardUtils.showSoftInput(editText);
            }
        }, 200);

    }

    @Override
    protected void onPositiveClick() {
        if (!checkData()) {
            return;
        }

        TextInputEditText editText = getDialogView().findViewById(R.id.password);
        String password = editText.getText().toString();

        //verify password
        getViewModel().verifyPwd(account, repoId, password);
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException seafException) {
                if (seafException != null) {
                    ToastUtils.showLong(seafException.getMessage());
                }
            }
        });

        getViewModel().getActionResultLiveData().observe(this, new Observer<TResultModel<RepoModel>>() {
            @Override
            public void onChanged(TResultModel<RepoModel> tResultModel) {
                if (tResultModel.success) {

                    if (resultListener != null) {
                        resultListener.onResultData(tResultModel.data);
                    }

                    dismissDialogWithIme();
                } else if (!TextUtils.isEmpty(tResultModel.error_msg)) {
                    ToastUtils.showLong(tResultModel.error_msg);
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

        return true;
    }
}
