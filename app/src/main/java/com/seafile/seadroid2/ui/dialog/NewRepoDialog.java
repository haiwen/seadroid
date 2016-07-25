package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
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
    private EditText mPasswordText;

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

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_new_repo, null);
        mRepoNameText = (EditText) view.findViewById(R.id.new_repo_name);
        mPasswordText = (EditText) view.findViewById(R.id.new_repo_password);

        if (savedInstanceState != null) {
            // Restore state
            mAccount = (Account) savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

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
    }

    @Override
    protected NewRepoTask prepareTask() {
        return new NewRepoTask(getRepoName(), getPassword(), getDataManager());
    }

    @Override
    protected void disableInput() {
        super.disableInput();
        mRepoNameText.setEnabled(false);
        mPasswordText.setEnabled(false);
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        mRepoNameText.setEnabled(true);
        mPasswordText.setEnabled(true);
    }
}
