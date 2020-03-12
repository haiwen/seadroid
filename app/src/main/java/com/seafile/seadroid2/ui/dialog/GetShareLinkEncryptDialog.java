package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;

import com.seafile.seadroid2.R;

class GetShareLinkPasswordTask extends TaskDialog.Task {
    String password;
    String days;

    public GetShareLinkPasswordTask(String password, String days) {
        this.password = password;
        this.days = days;
    }

    @Override
    protected void runTask() {

    }

    public String getPassword() {
        return password;
    }

    public String getDays() {
        return days;
    }
}


public class GetShareLinkEncryptDialog extends TaskDialog implements CompoundButton.OnCheckedChangeListener {
    private EditText passwordText;
    private EditText days;
    private CheckBox cbExpiration;


    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_share_password, null);
        passwordText = (EditText) view.findViewById(R.id.password);
        days = (EditText) view.findViewById(R.id.days);
        cbExpiration = (CheckBox) view.findViewById(R.id.add_expiration);
        cbExpiration.setOnCheckedChangeListener(this);
        return view;
    }


    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(getString(R.string.share_input_password));
        dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {

    }

    @Override
    protected void onValidateUserInput() throws Exception {
        String password = passwordText.getText().toString().trim();
        String day = days.getText().toString().trim();

        if (password.length() == 0) {
            String err = getActivity().getResources().getString(R.string.password_empty);
            throw new Exception(err);
        }

        if (password.length() < getResources().getInteger(R.integer.minimum_password_length)) {
            throw new Exception(getResources().getString(R.string.err_passwd_too_short));
        }

        if (cbExpiration.isChecked() && day.length() == 0) {
            String err = getActivity().getResources().getString(R.string.input_auto_expiration);
            throw new Exception(err);
        }
    }

    @Override
    protected void disableInput() {
        super.disableInput();
        passwordText.setEnabled(false);
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        passwordText.setEnabled(true);
    }


    @Override
    protected boolean executeTaskImmediately() {
        return false;
    }

    @Override
    protected GetShareLinkPasswordTask prepareTask() {
        String days = null;
        String password = passwordText.getText().toString().trim();
        if (cbExpiration.isChecked()) {
            days = this.days.getText().toString().trim();
        }
        GetShareLinkPasswordTask task = new GetShareLinkPasswordTask(password, days);
        return task;
    }


    @Override
    public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
        if (errorIsVisible()) {
            hideError();
        }
        if (isChecked) {
            days.setVisibility(View.VISIBLE);
        } else {
            days.setVisibility(View.GONE);

        }
    }

    public String getPassword() {
        if (getTask() != null) {
            GetShareLinkPasswordTask task = (GetShareLinkPasswordTask) getTask();
            return task.getPassword();
        }
        return null;
    }

    public String getDays() {
        if (getTask() != null) {
            GetShareLinkPasswordTask task = (GetShareLinkPasswordTask) getTask();
            return task.getDays();
        }
        return null;
    }
}