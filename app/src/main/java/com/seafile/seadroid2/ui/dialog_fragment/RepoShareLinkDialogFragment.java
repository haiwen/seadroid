package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.RadioGroup;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.TimeUtils;
import com.google.android.material.datepicker.MaterialDatePicker;
import com.google.android.material.datepicker.MaterialPickerOnPositiveButtonClickListener;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.materialswitch.MaterialSwitch;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.model.dirents.DirentPermissionModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.listener.Callback;
import com.seafile.seadroid2.listener.OnCreateDirentShareLinkListener;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.GetShareLinkPasswordViewModel;

import org.apache.commons.lang3.StringUtils;

public class RepoShareLinkDialogFragment extends RequestBottomSheetDialogFragmentWithVM<GetShareLinkPasswordViewModel> {

    private String repoId;
    private OnCreateDirentShareLinkListener onCreateDirentShareLinkListener;
    private final String expirationFormat = "yyyy/MM/dd";
    private Long selectedExpirationDateLong;

    public static RepoShareLinkDialogFragment newInstance(String repoId) {
        Bundle args = new Bundle();
        args.putString("repoId", repoId);
        RepoShareLinkDialogFragment fragment = new RepoShareLinkDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    public void setOnSharedListener(OnCreateDirentShareLinkListener onCreateDirentShareLinkListener) {
        this.onCreateDirentShareLinkListener = onCreateDirentShareLinkListener;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle bundle = getArguments();
        if (bundle == null || !bundle.containsKey("repoId")) {
            throw new NullPointerException("repoId is null");
        }

        repoId = bundle.getString("repoId");
        if (StringUtils.isEmpty(repoId)) {
            throw new NullPointerException("repoId is null");
        }

    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_repo_share_link;
    }

    @Override
    protected String getTitle() {
        return getContext().getString(R.string.generating_link_title);
    }

    @Override
    protected void onPositiveClick() {

        if (!checkData()) {
            return;
        }

        RadioGroup radioGroup = getDialogView().findViewById(R.id.radio_group);
        int cid = radioGroup.getCheckedRadioButtonId();

        DirentPermissionModel permissionModel = new DirentPermissionModel();
        if (cid == R.id.radio_group_1) {// preview and download
            permissionModel.can_edit = false;
            permissionModel.can_download = true;
            permissionModel.can_upload = false;
        } else if (cid == R.id.radio_group_2) {// only preview
            permissionModel.can_edit = false;
            permissionModel.can_download = false;
            permissionModel.can_upload = false;
        } else if (cid == R.id.radio_group_3) {// download and upload
            permissionModel.can_edit = false;
            permissionModel.can_download = true;
            permissionModel.can_upload = true;
        }

        getViewModel().createShareLink(repoId, "/", getPassword(), "all_users", "", selectedExpirationDateLong, permissionModel);
    }

    public String getPassword() {
        TextInputEditText editText = getDialogView().findViewById(R.id.password);
        return editText.getText().toString();
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        TextInputLayout passwordTextInput = getDialogView().findViewById(R.id.password_input_layout);

        MaterialSwitch passwordSwitch = getDialogView().findViewById(R.id.add_password);
        passwordSwitch.setOnCheckedChangeListener((buttonView, isChecked) -> {
            passwordTextInput.setVisibility(isChecked ? View.VISIBLE : View.GONE);
        });

        TextInputLayout daysTextInput = getDialogView().findViewById(R.id.days_text_input);
        TextInputEditText daysTextView = getDialogView().findViewById(R.id.days);
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
            TextInputEditText editText = getDialogView().findViewById(R.id.password);
            String password = editText.getText().toString();

            if (TextUtils.isEmpty(password)) {
                Toasts.show(R.string.password_empty);
                return false;
            }
        }

        MaterialSwitch daysSwitch = getDialogView().findViewById(R.id.add_expiration);
        if (daysSwitch.isChecked()) {
            TextInputEditText daysEditText = getDialogView().findViewById(R.id.days);
            String daysText = daysEditText.getText().toString();

            if (TextUtils.isEmpty(daysText) || selectedExpirationDateLong == null) {
                Toasts.show(R.string.tip_select_an_expiration_dates);
                return false;
            }
        }

        return true;
    }
}
