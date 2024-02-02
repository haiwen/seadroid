package com.seafile.seadroid2.ui.dialog_fragment;

import android.text.TextUtils;
import android.view.View;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.LinearLayout;

import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.switchmaterial.SwitchMaterial;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.GetShareLinkPasswordViewModel;

public class GetShareLinkPasswordDialogFragment extends RequestCustomDialogFragmentWithVM<GetShareLinkPasswordViewModel> {

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_share_password;
    }

    @Override
    protected void onPositiveClick() {
        dismiss();
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.share_input_password;
    }

    public String getPassword() {
        EditText editText = getDialogView().findViewById(R.id.password);
        String password = editText.getText().toString();
        return password;
    }

    public String getDays() {
        EditText daysEditText = getDialogView().findViewById(R.id.days);
        String daysText = daysEditText.getText().toString();
        return daysText;
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        TextInputLayout daysTextInput = getDialogView().findViewById(R.id.days_text_input);

        SwitchMaterial switchMaterial = getDialogView().findViewById(R.id.add_expiration);
        switchMaterial.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                if (isChecked) {
                    daysTextInput.setVisibility(View.VISIBLE);
                } else {
                    daysTextInput.setVisibility(View.GONE);
                }
            }
        });
    }

    private boolean checkData() {
        EditText editText = getDialogView().findViewById(R.id.password);
        String password = editText.getText().toString();

        if (TextUtils.isEmpty(password)) {
            ToastUtils.showLong(R.string.password_empty);
            return false;
        }

        //TODO 密码长度待确定
        if (password.length() < getResources().getInteger(R.integer.minimum_password_length)) {
            ToastUtils.showLong(R.string.err_passwd_too_short);
            return false;
        }

        SwitchMaterial switchMaterial = getDialogView().findViewById(R.id.add_expiration);
        if (switchMaterial.isChecked()) {
            EditText daysEditText = getDialogView().findViewById(R.id.days);
            String daysText = daysEditText.getText().toString();

            if (TextUtils.isEmpty(daysText)) {
                ToastUtils.showLong(R.string.input_auto_expiration);
                return false;
            }
        }

        return true;
    }
}
