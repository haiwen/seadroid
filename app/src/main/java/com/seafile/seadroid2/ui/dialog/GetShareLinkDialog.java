package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;

class GetShareLinkTask extends TaskDialog.Task {
    String repoID;
    String path;
    String password;
    boolean isdir;
    SeafConnection conn;
    String link;

    public GetShareLinkTask(String repoID, String path, String password, boolean isdir, SeafConnection conn) {
        this.repoID = repoID;
        this.path = path;
        this.password = password;
        this.isdir = isdir;
        this.conn = conn;
    }

    @Override
    protected void runTask() {
        try {
            link = conn.getShareLink(repoID, path, password, isdir);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }

    public String getResult() {
        return link;
    }
}

public class GetShareLinkDialog extends TaskDialog {
    private static final String STATE_TASK_REPO_NAME = "state_task_repo_name_share";
    private static final String STATE_TASK_REPO_ID = "state_task_repo_id_share";
    private String repoID;
    private String path;
    private boolean isEncrypt = false;
    private boolean isdir;
    private SeafConnection conn;
    private EditText passwordText;
    private String repoName;

    public void init(String repoID, String path, boolean isEncrypt, boolean isdir, Account account) {
        this.repoID = repoID;
        this.path = path;
        this.isEncrypt = isEncrypt;
        this.isdir = isdir;
        this.conn = new SeafConnection(account);
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = null;
        if (isEncrypt) {
            view = inflater.inflate(R.layout.dialog_password, null);
            passwordText = (EditText) view.findViewById(R.id.password);
            if (savedInstanceState != null) {
                repoName = savedInstanceState.getString(STATE_TASK_REPO_NAME);
                repoID = savedInstanceState.getString(STATE_TASK_REPO_ID);
            }
        }
        return view;
    }


    @Override
    protected void onDialogCreated(Dialog dialog) {
        if (isEncrypt) {
            dialog.setTitle(getString(R.string.share_input_password));
            dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
        } else {
            dialog.setTitle(getActivity().getString(R.string.generating_link));
        }
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        outState.putString(STATE_TASK_REPO_NAME, repoName);
        outState.putString(STATE_TASK_REPO_ID, repoID);
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        String password = passwordText.getText().toString().trim();

        if (password.length() == 0) {
            String err = getActivity().getResources().getString(R.string.password_empty);
            throw new Exception(err);
        }

        if (password.length() < getResources().getInteger(R.integer.minimum_password_length)) {
            throw new Exception(getResources().getString(R.string.err_passwd_too_short));
        }

    }

    @Override
    protected void disableInput() {
        super.disableInput();
        if (isEncrypt) {
            passwordText.setEnabled(false);
        }
    }

    @Override
    protected void enableInput() {
        super.enableInput();
        if (isEncrypt) {
            passwordText.setEnabled(true);
        }
    }


    @Override
    protected boolean executeTaskImmediately() {
        return !isEncrypt;
    }

    @Override
    protected GetShareLinkTask prepareTask() {
        String password = null;
        if (isEncrypt) {
            password = passwordText.getText().toString().trim();
        }
        GetShareLinkTask task = new GetShareLinkTask(repoID, path, password, isdir, conn);
        return task;
    }

    public String getLink() {
        if (getTask() != null) {
            GetShareLinkTask task = (GetShareLinkTask)getTask();
            return task.getResult();
        }

        return null;
    }
}