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
import com.seafile.seadroid2.ui.dialog.TaskDialog.Task;

class NewDirTask extends TaskDialog.Task {
    String repoID;
    String parentDir;
    String dirName;
    DataManager dataManager;

    public NewDirTask(String repoID, String parentDir,
                      String dirName, DataManager dataManager) {
        this.repoID = repoID;
        this.parentDir = parentDir;
        this.dirName = dirName;
        this.dataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            dataManager.createNewDir(repoID, parentDir, dirName);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class NewDirDialog extends TaskDialog {

    private static final String STATE_TASK_REPO_ID = "new_dir_task.repo_id";
    private static final String STATE_TASK_PARENT_DIR = "new_dir_task.parent_dir";
    private static final String STATE_ACCOUNT = "new_dir_task.account.account";

    private EditText dirNameText;
    private DataManager dataManager;
    private Account account;

    private String repoID;
    private String parentDir;

    public String getNewDirName() {
        return dirNameText.getText().toString().trim();
    }

    public void init(String repoID, String parentDir, Account account) {
        this.repoID = repoID;
        this.parentDir = parentDir;
        this.account = account;
    }

    private DataManager getDataManager() {
        if (dataManager == null) {
            dataManager = new DataManager(account);
        }

        return dataManager;
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_new_dir, null);
        dirNameText = (EditText) view.findViewById(R.id.new_dir_name);

        if (savedInstanceState != null) {
            repoID = savedInstanceState.getString(STATE_TASK_REPO_ID);
            parentDir = savedInstanceState.getString(STATE_TASK_PARENT_DIR);
            account = (Account)savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(getResources().getString(R.string.create_new_dir));
        dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        String dirName = dirNameText.getText().toString().trim();

        if (dirName.length() == 0) {
            String err = getActivity().getResources().getString(R.string.dir_name_empty);
            throw new Exception(err);
        }
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        outState.putString(STATE_TASK_PARENT_DIR, parentDir);
        outState.putString(STATE_TASK_REPO_ID, repoID);
        outState.putParcelable(STATE_ACCOUNT, account);
    }

    @Override
    protected void disableInput() {
        super.disableInput();
        dirNameText.setEnabled(false);
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        dirNameText.setEnabled(true);
    }

    @Override
    protected NewDirTask prepareTask() {
        EditText dirNameText = (EditText)getContentView().findViewById(R.id.new_dir_name);
        String dirName = dirNameText.getText().toString().trim();
        NewDirTask task = new NewDirTask(repoID, parentDir, dirName, getDataManager());
        return task;
    }
}