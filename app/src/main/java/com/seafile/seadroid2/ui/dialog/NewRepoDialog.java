package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.support.v4.widget.NestedScrollView;
import android.support.v7.widget.SwitchCompat;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.CompoundButton;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;

class NewRepoTask extends TaskDialog.Task {

    private String mRepoName;
    private String mPassword;
    private DataManager mDataManager;

    public NewRepoTask(String repoName, String password, DataManager dataManager) {
        mRepoName = repoName;
        mPassword = password;
        mDataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            mDataManager.createNewRepo(mRepoName, mPassword);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class NewRepoDialog extends TaskDialog {

    private final static String STATE_ACCOUNT = "new_repo_dialog.account";

    // The input fields of the dialog
    private EditText mRepoNameText;
    private SwitchCompat mEncryptSwitch;
    //  Use plain text field to avoid having to compare two obfuscated fields
    private EditText mPasswordText;
    private EditText mPasswordConfirmationText;
    private NestedScrollView mNestedScrollView;

    private Account mAccount;
    private DataManager mDataManager;

    public void init(Account account) {
        // The DataManager is not parcelable, so we save the intermediate Account instead
        mAccount = account;
    }

    private DataManager getDataManager() {
        if (mDataManager == null) {
            mDataManager = new DataManager(mAccount);
        }

        return mDataManager;
    }

    public String getRepoName() { return mRepoNameText.getText().toString().trim(); }
    private String getPassword() { return mPasswordText.getText().toString().trim(); }
    private String getPasswordConfirmation() { return mPasswordConfirmationText.getText().toString().trim(); }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_new_repo, null);
        mRepoNameText = (EditText) view.findViewById(R.id.new_repo_name);
        mEncryptSwitch = (SwitchCompat) view.findViewById(R.id.new_repo_encrypt_switch);
        mPasswordText = (EditText) view.findViewById(R.id.new_repo_password);
        mPasswordText.setHint(String.format(
            getResources().getString(R.string.passwd_min_len_limit_hint),
            getResources().getInteger(R.integer.minimum_password_length)
        ));
        mPasswordConfirmationText = (EditText) view.findViewById(R.id.new_repo_password_confirmation);
        mNestedScrollView = (NestedScrollView) view.findViewById(R.id.nsv_new_repo_container);

        if (savedInstanceState != null) {
            // Restore state
            mAccount = (Account) savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        mEncryptSwitch.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                if (isChecked) {
                    mNestedScrollView.setVisibility(View.VISIBLE);
                } else {
                    mNestedScrollView.setVisibility(View.GONE);

                    // Delete entered passwords so hiding the input fields creates an unencrypted repo
                    mPasswordText.setText("");
                    mPasswordConfirmationText.setText("");
                }
            }
        });

        return view;
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        // Save state
        outState.putParcelable(STATE_ACCOUNT, mAccount);
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(R.string.create_new_repo);
        dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        if (getRepoName().length() == 0) {
            throw new Exception(getResources().getString(R.string.repo_name_empty));
        }

        if (mEncryptSwitch.isChecked()) {
            if (getPassword().length() == 0) {
                throw new Exception(getResources().getString(R.string.err_passwd_empty));
            }

            if (getPassword().length() < getResources().getInteger(R.integer.minimum_password_length)) {
                throw new Exception(getResources().getString(R.string.err_passwd_too_short));
            }

            if (!getPassword().equals(getPasswordConfirmation())) {
                throw new Exception(getResources().getString(R.string.err_passwd_mismatch));
            }
        }
    }

    @Override
    protected NewRepoTask prepareTask() {
        return new NewRepoTask(getRepoName(), getPassword(), getDataManager());
    }

    @Override
    protected void disableInput() {
        super.disableInput();
        mRepoNameText.setEnabled(false);
        mEncryptSwitch.setEnabled(false);
        mPasswordText.setEnabled(false);
        mPasswordConfirmationText.setEnabled(false);
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        mRepoNameText.setEnabled(true);
        mEncryptSwitch.setEnabled(true);
        mPasswordText.setEnabled(true);
        mPasswordConfirmationText.setEnabled(true);
    }
}
