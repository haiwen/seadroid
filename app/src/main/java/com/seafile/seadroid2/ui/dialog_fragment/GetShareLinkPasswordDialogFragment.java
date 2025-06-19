package com.seafile.seadroid2.ui.dialog_fragment;

import android.text.TextUtils;
import android.view.View;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.blankj.utilcode.util.TimeUtils;
import com.google.android.material.datepicker.MaterialDatePicker;
import com.google.android.material.datepicker.MaterialPickerOnPositiveButtonClickListener;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.materialswitch.MaterialSwitch;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.listener.Callback;
import com.seafile.seadroid2.listener.OnCreateDirentShareLinkListener;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.GetShareLinkPasswordViewModel;

public class GetShareLinkPasswordDialogFragment extends RequestCustomDialogFragmentWithVM<GetShareLinkPasswordViewModel> {

    private String repoId, path;
    private boolean isAdvance = true;
    private OnCreateDirentShareLinkListener onCreateDirentShareLinkListener;
    private final String expirationFormat = "yyyy/MM/dd";
    private Long selectedExpirationDateLong;

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

            getViewModel().createShareLink(repoId, path, getPassword(), selectedExpirationDateLong, null);
        } else {
            getViewModel().getFirstShareLink(repoId, path);
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

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        if (isAdvance) {
            TextInputLayout passwordTextInput = getDialogView().findViewById(R.id.password_input_layout);
//            passwordTextInput.setHint(String.format(
//                    getResources().getString(R.string.passwd_min_len_limit_hint),
//                    getResources().getInteger(R.integer.minimum_password_length)
//            ));

            MaterialSwitch passwordSwitch = getDialogView().findViewById(R.id.add_password);
            passwordSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
                passwordTextInput.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            });

            TextInputLayout daysTextInput = getDialogView().findViewById(R.id.days_text_input);
            EditText daysTextView = getDialogView().findViewById(R.id.days);
            daysTextView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    showDatePicker(new Callback<Long>() {
                        @Override
                        public void callback(Long aLong) {
                            selectedExpirationDateLong = aLong;
                            String ymd = TimeUtils.millis2String(aLong, expirationFormat);
                            daysTextView.setText(ymd);
                        }
                    });
                }
            });

            MaterialSwitch daysSwitch = getDialogView().findViewById(R.id.add_expiration);
            daysSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
                daysTextInput.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            });
        }
    }

    private void showDatePicker(Callback<Long> call) {
        if (call == null) {
            throw new NullPointerException("call is null");
        }

        MaterialDatePicker<Long> picker = MaterialDatePicker.Builder.datePicker().build();
        picker.addOnPositiveButtonClickListener(new MaterialPickerOnPositiveButtonClickListener<Long>() {
            @Override
            public void onPositiveButtonClick(Long selection) {
                if (selection == null) return;
                call.callback(selection);
            }
        });
        picker.show(getChildFragmentManager(), MaterialDatePicker.class.getName());
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
        builder.setTitle(getErrorMsg(errMsg));
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

        MaterialSwitch passwordSwitch = getDialogView().findViewById(R.id.add_password);
        if (passwordSwitch.isChecked()) {
            EditText editText = getDialogView().findViewById(R.id.password);
            String password = editText.getText().toString();

            if (TextUtils.isEmpty(password)) {
                Toasts.show(R.string.password_empty);
                return false;
            }
        }

        MaterialSwitch daysSwitch = getDialogView().findViewById(R.id.add_expiration);
        if (daysSwitch.isChecked()) {
            EditText daysEditText = getDialogView().findViewById(R.id.days);
            String daysText = daysEditText.getText().toString();

            if (TextUtils.isEmpty(daysText) || selectedExpirationDateLong == null) {
                Toasts.show(R.string.tip_select_an_expiration_dates);
                return false;
            }
        }

        return true;
    }
}
