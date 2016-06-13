package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ui.dialog.TaskDialog.Task;

class NewFileTask extends TaskDialog.Task {
    String repoID;
    String parentDir;
    String fileName;
    DataManager dataManager;

    public NewFileTask(String repoID, String parentDir,
                       String fileName, DataManager dataManager) {
        this.repoID = repoID;
        this.parentDir = parentDir;
        this.fileName = fileName;
        this.dataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            dataManager.createNewFile(repoID, parentDir, fileName);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class NewFileDialog extends TaskDialog {
    private static final String STATE_TASK_REPO_ID = "new_file_task.repo_id";
    private static final String STATE_TASK_PARENT_DIR = "new_file_task.parent_dir";
    private static final String STATE_ACCOUNT = "new_file_task.account.account";

    private EditText fileNameText;
    private String repoID;
    private String parentDir;

    private DataManager dataManager;
    private Account account;

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

    public String getNewFileName() {
        return fileNameText.getText().toString().trim();
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_new_file, null);
        fileNameText = (EditText) view.findViewById(R.id.new_file_name);

        if (savedInstanceState != null) {
            repoID = savedInstanceState.getString(STATE_TASK_REPO_ID);
            parentDir = savedInstanceState.getString(STATE_TASK_PARENT_DIR);
            account = (Account)savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        return view;
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        outState.putString(STATE_TASK_PARENT_DIR, parentDir);
        outState.putString(STATE_TASK_REPO_ID, repoID);
        outState.putParcelable(STATE_ACCOUNT, account);
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(getResources().getString(R.string.create_new_file));
        dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        String fileName = fileNameText.getText().toString().trim();

        if (fileName.length() == 0) {
            String err = getActivity().getResources().getString(R.string.file_name_empty);
            throw new Exception(err);
        }
    }

    @Override
    protected NewFileTask prepareTask() {
        String fileName = fileNameText.getText().toString().trim();
        NewFileTask task = new NewFileTask(repoID, parentDir, fileName, getDataManager());
        return task;
    }

    @Override
    protected void disableInput() {
        super.disableInput();
        fileNameText.setEnabled(false);
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        fileNameText.setEnabled(true);
    }
}