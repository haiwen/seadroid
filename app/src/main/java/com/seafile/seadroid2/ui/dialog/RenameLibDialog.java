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

class RenameLibTask extends TaskDialog.Task {

    private String mRepoID;
    private String mCurrentName;
    private String mNewName;
    private DataManager mDataManager;

    public RenameLibTask(String repoID, String currentName, String newName, DataManager dataManager) {
        mRepoID = repoID;
        mCurrentName = currentName;
        mNewName = newName;
        mDataManager = dataManager;
    }

    @Override
    protected void runTask() {
        if (mNewName.equals(mCurrentName)) { return; }

        try {
            mDataManager.renameLib(mRepoID, mNewName);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class RenameLibDialog extends TaskDialog {

    private final static String STATE_REPO_ID = "rename_lib_dialog.repo_id";
    private final static String STATE_CURRENT_NAME = "rename_lib_dialog.current_name";
    private final static String STATE_ACCOUNT = "rename_lib_dialog.account";

    private String mRepoID;
    private String mCurrentName;
    private Account mAccount;
    private DataManager mDataManager;

    private EditText mLibNameText;

    public void init(String repoID, String currentName, Account account) {
        mRepoID = repoID;
        mCurrentName = currentName;
        mAccount = account;
    }

    private DataManager getDataManager() {
        if (mDataManager == null) {
            mDataManager = new DataManager(mAccount);
        }

        return mDataManager;
    }

    private String getNewName() { return mLibNameText.getText().toString().trim(); }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_new_file, null);
        mLibNameText = (EditText) view.findViewById(R.id.new_file_name);

        if (savedInstanceState != null) {
            mRepoID = savedInstanceState.getString(STATE_REPO_ID);
            mCurrentName = savedInstanceState.getString(STATE_CURRENT_NAME);
            mAccount = (Account)savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        mLibNameText.setText(mCurrentName);
        mLibNameText.setSelection(mCurrentName.length());

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        super.onDialogCreated(dialog);
        dialog.setTitle(R.string.rename_lib);
        dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        super.onSaveDialogContentState(outState);
        outState.putString(STATE_REPO_ID, mRepoID);
        outState.putString(STATE_CURRENT_NAME, mCurrentName);
        outState.putParcelable(STATE_ACCOUNT, mAccount);
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        super.onValidateUserInput();

        if (getNewName().length() == 0) {
            throw new Exception(getResources().getString(R.string.lib_name_empty));
        }

    }

    @Override
    protected void disableInput() {
        super.disableInput();
        mLibNameText.setEnabled(false);
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        mLibNameText.setEnabled(true);
    }

    @Override
    protected Task prepareTask() {
        return new RenameLibTask(mRepoID, mCurrentName, getNewName(), getDataManager());
    }
}

