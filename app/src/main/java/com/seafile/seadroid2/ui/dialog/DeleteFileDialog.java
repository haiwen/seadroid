package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ui.dialog.TaskDialog.Task;
import com.seafile.seadroid2.util.Utils;

class DeleteTask extends TaskDialog.Task {
    String repoID;
    String path;
    boolean isdir;
    DataManager dataManager;

    public DeleteTask(String repoID, String path, boolean isdir, DataManager dataManager) {
        this.repoID = repoID;
        this.path = path;
        this.isdir = isdir;
        this.dataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            dataManager.delete(repoID, path, isdir);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class DeleteFileDialog extends TaskDialog {
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

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_delete_file, null);
        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        String str = getActivity().getString(
                isdir ? R.string.delete_dir : R.string.delete_file_f);
        setTitle(str);
        // dialog.setTitle(str + " " + Utils.fileNameFromPath(path));
    }

    @Override
    protected DeleteTask prepareTask() {
        DeleteTask task = new DeleteTask(repoID, path, isdir, getDataManager());
        return task;
    }
}
