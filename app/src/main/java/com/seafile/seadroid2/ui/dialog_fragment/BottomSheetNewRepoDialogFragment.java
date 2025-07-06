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
import com.blankj.utilcode.util.NetworkUtils;
import com.google.android.material.materialswitch.MaterialSwitch;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.NewRepoViewModel;

public class BottomSheetNewRepoDialogFragment extends RequestBottomSheetDialogFragmentWithVM<NewRepoViewModel> {

    @Override
    protected int getLayoutId() {
        return R.layout.dialog_new_repo;
    }

    @Override
    protected String getTitle() {
        return getString(R.string.create_new_repo);
    }

    private int passwordMinLength = 4;

    @Override
    protected void initView(LinearLayout parentView) {
        super.initView(parentView);

        passwordMinLength = getResources().getInteger(R.integer.minimum_password_length);

        TextInputEditText inputEditText1 = getDialogView().findViewById(R.id.edit_text_pwd1);
        TextInputEditText inputEditText2 = getDialogView().findViewById(R.id.edit_text_pwd2);

        TextInputLayout inputLayout1 = getDialogView().findViewById(R.id.edit_text_pwd1_layout);
        inputLayout1.setEndIconOnClickListener(v -> {
            if (inputEditText1.getTransformationMethod() instanceof PasswordTransformationMethod) {
                inputEditText1.setTransformationMethod(HideReturnsTransformationMethod.getInstance());
                inputLayout1.setEndIconDrawable(R.drawable.icon_eye_open);
            } else {
                inputEditText1.setTransformationMethod(PasswordTransformationMethod.getInstance());
                inputLayout1.setEndIconDrawable(R.drawable.icon_eye_close);
            }

            String input = inputEditText1.getText().toString().trim();
            if (!TextUtils.isEmpty(input)) {
                inputEditText1.setSelection(input.length());
            }
        });

        TextInputLayout inputLayout2 = getDialogView().findViewById(R.id.edit_text_pwd2_layout);
        inputLayout2.setEndIconOnClickListener(v -> {
            if (inputEditText2.getTransformationMethod() instanceof PasswordTransformationMethod) {
                inputEditText2.setTransformationMethod(HideReturnsTransformationMethod.getInstance());
                inputLayout2.setEndIconDrawable(R.drawable.icon_eye_open);
            } else {
                inputEditText2.setTransformationMethod(PasswordTransformationMethod.getInstance());
                inputLayout2.setEndIconDrawable(R.drawable.icon_eye_close);
            }

            String input = inputEditText2.getText().toString().trim();
            if (!TextUtils.isEmpty(input)) {
                inputEditText2.setSelection(input.length());
            }
        });

        inputEditText1.setHint(getResources().getString(R.string.passwd_min_len_limit_hint, passwordMinLength));

        MaterialSwitch materialSwitch = parentView.findViewById(R.id.widget_switch);
        materialSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            inputLayout1.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            inputLayout2.setVisibility(isChecked ? View.VISIBLE : View.GONE);

            inputEditText1.setText(null);
            inputEditText2.setText(null);
        });
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        EditText editText = getDialogView().findViewById(R.id.edit_name);
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

    public void initViewModel() {
        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                Toasts.show(e.getMessage());
                refreshData(false);

                dismissDialogWithIme();
            }
        });

        getViewModel().getCreateRepoLiveData().observe(this, repoModel -> {
            String d = String.format(getResources().getString(R.string.create_new_repo_success), repoModel.repo_name);
            Toasts.show(d);

            refreshData();

            dismissDialogWithIme();
        });

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);
    }

    @Override
    protected void onPositiveClick() {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            return;
        }

        if (!checkData()) {
            return;
        }

        EditText name = getDialogView().findViewById(R.id.edit_name);



        MaterialSwitch materialSwitch = getDialogView().findViewById(R.id.widget_switch);
        if (materialSwitch.isChecked()) {

            TextInputEditText inputEditText1 = getDialogView().findViewById(R.id.edit_text_pwd1);
            String pwd1Str = inputEditText1.getText() == null ? "" : inputEditText1.getText().toString();

            String nameStr = name.getText() == null ? "" : name.getText().toString();
            nameStr = StringUtils.trimEnd(nameStr, " ");
            getViewModel().createNewRepo(nameStr, "", pwd1Str);
        } else {
            String nameStr = name.getText() == null ? "" : name.getText().toString();
            nameStr = StringUtils.trimEnd(nameStr, " ");
            getViewModel().createNewRepo(nameStr, "", "");
        }
    }

    private boolean checkData() {

        EditText name = getDialogView().findViewById(R.id.edit_name);
        Editable editable = name.getText();
        if (editable == null || editable.length() == 0 || TextUtils.isEmpty(editable.toString().trim())) {
            Toasts.show(R.string.repo_name_empty);
            return false;
        }

        MaterialSwitch materialSwitch = getDialogView().findViewById(R.id.widget_switch);
        if (!materialSwitch.isChecked()) {
            return true;
        }


        TextInputEditText inputEditText1 = getDialogView().findViewById(R.id.edit_text_pwd1);
        TextInputEditText inputEditText2 = getDialogView().findViewById(R.id.edit_text_pwd2);

        Editable editable1 = inputEditText1.getText();
        Editable editable2 = inputEditText2.getText();
        boolean editableBool1 = editable1 == null || editable1.length() == 0 || TextUtils.isEmpty(editable1.toString().trim());
        boolean editableBool2 = editable2 == null || editable2.length() == 0 || TextUtils.isEmpty(editable2.toString().trim());

        if (editableBool1 || editableBool2) {
            Toasts.show(R.string.err_passwd_empty);
            return false;
        }

        if (editable1.length() < passwordMinLength) {
            Toasts.show(R.string.err_passwd_too_short);
            return false;
        }

        if (!TextUtils.equals(editable1, editable2)) {
            Toasts.show(R.string.err_passwd_mismatch);
            return false;
        }
        return true;
    }
}
