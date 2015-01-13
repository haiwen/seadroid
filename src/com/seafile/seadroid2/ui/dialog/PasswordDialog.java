package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ui.dialog.TaskDialog.Task;

class SetPasswordTask extends TaskDialog.Task {
    String repoID;
    String password;
    DataManager dataManager;

    public SetPasswordTask(String repoID, String password,
                           DataManager dataManager) {
        this.repoID = repoID;
        this.password = password;
        this.dataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            dataManager.setPassword(repoID, password);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class PasswordDialog extends TaskDialog {
    private static final String STATE_TASK_REPO_NAME = "set_password_task.repo_name";
    private static final String STATE_TASK_REPO_ID = "set_password_task.repo_id";
    private static final String STATE_TASK_PASSWORD = "set_password_task.password";
    private static final String STATE_ACCOUNT = "set_password_task.account";

    private EditText passwordText;
    private String repoID, repoName;
    private DataManager dataManager;
    private Account account;
    private String password;

    public void setRepo(String repoName, String repoID, Account account) {
        this.repoName = repoName;
        this.repoID = repoID;
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
        View view = inflater.inflate(R.layout.dialog_password, null);
        passwordText = (EditText) view.findViewById(R.id.password);

        if (savedInstanceState != null) {
            repoName = savedInstanceState.getString(STATE_TASK_REPO_NAME);
            repoID = savedInstanceState.getString(STATE_TASK_REPO_ID);
            account = (Account)savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        if (password != null) {
            passwordText.setText(password);
        }

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        setTitle(repoName);
        // setTitle(String.format(getString(R.string.provide_password), repoName));
        // dialog.setTitle(String.format(getString(R.string.provide_password), repoName));
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        outState.putString(STATE_TASK_REPO_NAME, repoName);
        outState.putString(STATE_TASK_REPO_ID, repoID);
        outState.putParcelable(STATE_ACCOUNT, account);
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        String password = passwordText.getText().toString().trim();

        if (password.length() == 0) {
            String err = getActivity().getResources().getString(R.string.password_empty);
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
    protected SetPasswordTask prepareTask() {
        String password = passwordText.getText().toString().trim();
        SetPasswordTask task = new SetPasswordTask(repoID, password, getDataManager());
        return task;
    }

    @Override
    protected void onSaveTaskState(Bundle outState) {
        SetPasswordTask task = (SetPasswordTask)getTask();
        if (task != null) {
            outState.putString(STATE_TASK_PASSWORD, task.password);
        }
    }

    @Override
    protected SetPasswordTask onRestoreTaskState(Bundle outState) {
        if (outState == null) {
            return null;
        }

        String password = outState.getString(STATE_TASK_PASSWORD);
        if (password != null) {
            return new SetPasswordTask(repoID, password, getDataManager());
        } else {
            return null;
        }
    }

    public void setPassword(String password) {
        this.password = password;
    }

    @Override
    protected boolean executeTaskImmediately() {
        return password != null;
    }

    @Override
    public void onTaskSuccess() {
        String password = passwordText.getText().toString().trim();
        DataManager.setRepoPasswordSet(repoID, password);
        super.onTaskSuccess();
    }

    @Override
    protected String getErrorFromException(SeafException e) {
        if (e.getCode() == 400) {
            return getString(R.string.wrong_password);
        }
        return e.getMessage();
    }
}
