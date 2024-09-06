package com.seafile.seadroid2.ui.dialog_fragment;

import android.text.TextUtils;
import android.view.View;
import android.widget.EditText;
import android.widget.LinearLayout;

import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.switchmaterial.SwitchMaterial;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.listener.OnCreateDirentShareLinkListener;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.GetShareLinkPasswordViewModel;

public class GetShareLinkPasswordDialogFragment extends RequestCustomDialogFragmentWithVM<GetShareLinkPasswordViewModel> {

    private String repoId, path;
    private boolean isAdvance = true;
    private OnCreateDirentShareLinkListener onCreateDirentShareLinkListener;

    public void init(String repoId, String path, boolean isAdvance) {
        this.repoId = repoId;
        this.path = path;
        this.isAdvance = isAdvance;
    }

    public void setOnCreateDirentShareLinkListener(OnCreateDirentShareLinkListener onCreateDirentShareLinkListener) {
        this.onCreateDirentShareLinkListener = onCreateDirentShareLinkListener;
    }

    @Override
    protected int getLayoutId() {
        return isAdvance ? R.layout.view_dialog_share_password : R.layout.view_dialog_share_no_password;
    }

    private boolean isOping = false;

    @Override
    public void onResume() {
        super.onResume();

        if (!isOping && !isAdvance) {
            onPositiveClick();
            isOping = true;
        }
    }

    @Override
    protected void onPositiveClick() {
        if (isAdvance) {
            if (!checkData()) {
                return;
            }

            getViewModel().createShareLink(repoId, path, getPassword(), getDays(), null);
        } else {
            getViewModel().getFirstShareLink(repoId, path, null, null);
        }
    }

    @Override
    public int getDialogTitleRes() {
        return isAdvance ? R.string.generating_link_title : R.string.generating_link;
    }

    public String getPassword() {
        EditText editText = getDialogView().findViewById(R.id.password);
        return editText.getText().toString();
    }

    public String getDays() {
        EditText daysEditText = getDialogView().findViewById(R.id.days);
        return daysEditText.getText().toString();
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        if (isAdvance) {
            TextInputLayout passwordTextInput = getDialogView().findViewById(R.id.password_input_layout);
//            passwordTextInput.setHint(String.format(
//                    getResources().getString(R.string.passwd_min_len_limit_hint),
//                    getResources().getInteger(R.integer.minimum_password_length)
//            ));

            SwitchMaterial passwordSwitch = getDialogView().findViewById(R.id.add_password);
            passwordSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
                passwordTextInput.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            });

            TextInputLayout daysTextInput = getDialogView().findViewById(R.id.days_text_input);
            SwitchMaterial switchMaterial = getDialogView().findViewById(R.id.add_expiration);
            switchMaterial.setOnCheckedChangeListener((buttonView, isChecked) -> {
                daysTextInput.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            });
        }
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);

        getViewModel().getSeafExceptionLiveData().observe(this, e -> {
            showErrorDialog(e.getMessage());
            dismiss();
        });

        getViewModel().getLinkLiveData().observe(this, direntShareLinkModel -> {
            if (onCreateDirentShareLinkListener != null) {
                onCreateDirentShareLinkListener.onCreateDirentShareLink(direntShareLinkModel);
            }
        });
    }

    private void showErrorDialog(String errMsg) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
        builder.setMessage(getErrorMsg(errMsg));
        builder.setPositiveButton(R.string.ok, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

    private String getErrorMsg(String errMsg) {
        if (errMsg.contains("Password is too short")) {
            return getString(R.string.err_passwd_too_short);
        }
        return errMsg;
    }

    private boolean checkData() {

        SwitchMaterial passwordSwitch = getDialogView().findViewById(R.id.add_password);
        if (passwordSwitch.isChecked()) {
            EditText editText = getDialogView().findViewById(R.id.password);
            String password = editText.getText().toString();

            if (TextUtils.isEmpty(password)) {
                ToastUtils.showLong(R.string.password_empty);
                return false;
            }
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
