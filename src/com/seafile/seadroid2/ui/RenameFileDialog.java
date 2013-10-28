package com.seafile.seadroid2.ui;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;

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

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        String str = getActivity().getString(isdir ? R.string.rename_dir : R.string.rename_file);
        dialog.setTitle(str + " " + Utils.fileNameFromPath(path));
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
}