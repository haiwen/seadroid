package com.seafile.seadroid2.ui.dialog_fragment;

import android.text.Editable;
import android.text.TextUtils;
import android.view.View;
import android.widget.LinearLayout;

import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.switchmaterial.SwitchMaterial;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.NewRepoViewModel;

public class NewRepoDialogFragment extends RequestCustomDialogFragmentWithVM<NewRepoViewModel> {

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_new_repo;
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.create_new_repo;
    }

    @Override
    public void initView(LinearLayout containerView) {
        super.initView(containerView);
        
        SwitchMaterial materialSwitch = getDialogView().findViewById(R.id.widget_switch);
        TextInputLayout pwd1 = getDialogView().findViewById(R.id.new_repo_input_layout_pwd_1);
        TextInputLayout pwd2 = getDialogView().findViewById(R.id.new_repo_input_layout_pwd_2);
        pwd1.setHint(String.format(
                getResources().getString(R.string.passwd_min_len_limit_hint),
                getResources().getInteger(R.integer.minimum_password_length)
        ));

        TextInputEditText pwdt1 = getDialogView().findViewById(R.id.new_repo_edit_pwd_1);
        TextInputEditText pwdt2 = getDialogView().findViewById(R.id.new_repo_edit_pwd_2);

        materialSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            pwd1.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            pwd2.setVisibility(isChecked ? View.VISIBLE : View.GONE);

            pwdt1.setText(null);
            pwdt2.setText(null);
        });
    }

    @Override
    public void initViewModel() {
        getViewModel().getCreateRepoLiveData().observe(this, repoModel -> {
            String d = String.format(getResources().getString(R.string.create_new_repo_success), repoModel.repo_name);
            ToastUtils.showLong(d);

            refreshData();

            dismiss();
        });

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);
    }

    @Override
    protected void onPositiveClick() {
        if (!checkData()) {
            return;
        }

        TextInputEditText name = getDialogView().findViewById(R.id.new_repo_edit_name);
        SwitchMaterial materialSwitch = getDialogView().findViewById(R.id.widget_switch);
        if (materialSwitch.isChecked()) {
            TextInputEditText pwd1 = getDialogView().findViewById(R.id.new_repo_edit_pwd_1);
            String pwd1Str = pwd1.getText() == null ? "" : pwd1.getText().toString();
            String nameStr = name.getText() == null ? "" : name.getText().toString();
            getViewModel().createNewRepo(nameStr, "", pwd1Str);
        } else {
            String nameStr = name.getText() == null ? "" : name.getText().toString();
            getViewModel().createNewRepo(nameStr, "", "");
        }
    }

    private boolean checkData() {

        TextInputEditText name = getDialogView().findViewById(R.id.new_repo_edit_name);
        Editable editable = name.getText();
        if (editable == null || editable.length() == 0 || TextUtils.isEmpty(editable.toString().trim())) {
            ToastUtils.showLong(R.string.repo_name_empty);
            return false;
        }

        SwitchMaterial materialSwitch = getDialogView().findViewById(R.id.widget_switch);
        if (!materialSwitch.isChecked()) {
            return true;
        }

        TextInputEditText pwd1 = getDialogView().findViewById(R.id.new_repo_edit_pwd_1);
        TextInputEditText pwd2 = getDialogView().findViewById(R.id.new_repo_edit_pwd_2);

        Editable editable1 = pwd1.getText();
        Editable editable2 = pwd2.getText();
        boolean editableBool1 = editable1 == null || editable1.length() == 0 || TextUtils.isEmpty(editable1.toString().trim());
        boolean editableBool2 = editable2 == null || editable2.length() == 0 || TextUtils.isEmpty(editable2.toString().trim());

        if (editableBool1 || editableBool2) {
            ToastUtils.showLong(R.string.err_passwd_empty);
            return false;
        }

        if (editable1.length() < Constants.PASSWORD_MINIMUM_LENGTH) {
            ToastUtils.showLong(R.string.err_passwd_too_short);
            return false;
        }

        if (!TextUtils.equals(editable1, editable2)) {
            ToastUtils.showLong(R.string.err_passwd_mismatch);
            return false;
        }
        return true;
    }
}
