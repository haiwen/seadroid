package com.seafile.seadroid2.ui;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;

class SetPasswordTask extends TaskDialog.Task {
    private String repoID;
    private String password;
    private DataManager dataManager;

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

public class PasswordDialog extends TaskDialog<SetPasswordTask> {
    private EditText passwordText;
    private String repoID, repoName;

    public void setRepo(String repoName, String repoID) {
        this.repoName = repoName;
        this.repoID = repoID;
    }

    @Override
    protected View onCreateDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_password, null);
        passwordText = (EditText) view.findViewById(R.id.password);

        if (savedInstanceState != null) {
        }

        return view;
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        String password = passwordText.getText().toString().trim();

        if (password.length() == 0) {
            String err = getBrowserActivity().getResources().getString(R.string.password_empty);
            throw new Exception(err);
        }
    }

    @Override
    protected void saveDialogState(Bundle outState) {
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
        SetPasswordTask task = new SetPasswordTask(repoID, password,
                                                   getBrowserActivity().getDataManager());
        return task;
    }
}
