package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;

class DeleteLibTask extends TaskDialog.Task {

    private String mRepoID;
    private DataManager mDataManager;

    public DeleteLibTask(String repoID, DataManager dataManager) {
        mRepoID = repoID;
        mDataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            mDataManager.deleteLib(mRepoID);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class DeleteLibDialog extends TaskDialog {

    private final static String STATE_REPO_ID = "delete_lib_dialog.repo_id";
    private final static String STATE_ACCOUNT = "delete_lib_dialog.account";

    private String mRepoID;
    private Account mAccount;
    private DataManager mDataManager;

    public void init(String repoID, Account account) {
        mRepoID = repoID;
        mAccount = account;
    }

    private DataManager getDataManager() {
        if (mDataManager == null) {
            mDataManager = new DataManager(mAccount);
        }

        return mDataManager;
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_delete_lib, null);

        if (savedInstanceState != null) {
            // Restore state
            mRepoID = savedInstanceState.getString(STATE_REPO_ID);
            mAccount = savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        return view;
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        super.onSaveDialogContentState(outState);
        outState.putString(STATE_REPO_ID, mRepoID);
        outState.putParcelable(STATE_ACCOUNT, mAccount);
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        super.onDialogCreated(dialog);
        dialog.setTitle(R.string.delete_lib_title);
    }

    @Override
    protected DeleteLibTask prepareTask() {
        return new DeleteLibTask(mRepoID, getDataManager());
    }
}
