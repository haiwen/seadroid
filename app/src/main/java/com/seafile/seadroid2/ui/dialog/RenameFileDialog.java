package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ui.dialog.TaskDialog.Task;
import com.seafile.seadroid2.util.Utils;

class RenameTask extends TaskDialog.Task {
    String repoID;
    String path;
    String newName;
    boolean isdir;
    DataManager dataManager;

    public RenameTask(String repoID, String path,
                      String newName, boolean isdir, DataManager dataManager) {
        this.repoID = repoID;
        this.path = path;
        this.newName = newName;
        this.isdir = isdir;
        this.dataManager = dataManager;
    }

    @Override
    protected void runTask() {
        if (newName.equals(Utils.fileNameFromPath(path))) {
            return;
        }
        try {
            dataManager.rename(repoID, path, newName, isdir);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class RenameFileDialog extends TaskDialog {
    private EditText fileNameText;
    private String repoID;
    private String path;
    private boolean isdir;

    private DataManager dataManager;
    private Account account;

    private static final String STATE_REPO_ID = "rename_task.repo_name";
    private static final String STATE_PATH = "rename_task.repo_id";
    private static final String STATE_ISDIR = "rename_task.account";
    private static final String STATE_ACCOUNT = "rename_task.account";

    public void init(String repoID, String path, boolean isdir, Account account) {
        this.repoID = repoID;
        this.path = path;
        this.isdir = isdir;
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
            repoID = savedInstanceState.getString(STATE_REPO_ID);
            path = savedInstanceState.getString(STATE_PATH);
            isdir = savedInstanceState.getBoolean(STATE_ISDIR);
            account = (Account)savedInstanceState.getParcelable(STATE_ACCOUNT);
        }

        final String fileName = Utils.fileNameFromPath(path);
        if (!TextUtils.isEmpty(fileName)) {
            fileNameText.setText(fileName);
            fileNameText.setSelection(fileName.length());
        }

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        String str = getActivity().getString(isdir ? R.string.rename_dir : R.string.rename_file);
        dialog.setTitle(str);
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
    protected RenameTask prepareTask() {
        String newName = fileNameText.getText().toString().trim();

        RenameTask task = new RenameTask(repoID, path, newName, isdir, getDataManager());
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

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
        outState.putString(STATE_REPO_ID, repoID);
        outState.putString(STATE_PATH, path);
        outState.putBoolean(STATE_ISDIR, isdir);
        outState.putParcelable(STATE_ACCOUNT, account);
    }
}
