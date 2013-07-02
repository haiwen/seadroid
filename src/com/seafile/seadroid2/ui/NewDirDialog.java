package com.seafile.seadroid2.ui;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;

import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;

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

    private EditText dirNameText;

    public String getNewDirName() {
        return dirNameText.getText().toString().trim();
    }

    @Override
    protected View onCreateDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_new_dir, null);
        dirNameText = (EditText) view.findViewById(R.id.new_dir_name);

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle("Create a new folder");
    }

    @Override
    protected void onValidateUserInput() throws Exception {
        String dirName = dirNameText.getText().toString().trim();

        if (dirName.length() == 0) {
            String err = getBrowserActivity().getResources().getString(R.string.dir_name_empty);
            throw new Exception(err);
        }
    }

    @Override
    protected void onSaveDialogContentState(Bundle outState) {
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
        NavContext nav = getBrowserActivity().getNavContext();
        NewDirTask task = new NewDirTask(nav.getRepoID(), nav.getDirPath(), dirName,
                                         getBrowserActivity().getDataManager());
        return task;
    }

    @Override
    protected void onSaveTaskState(Bundle outState) {
    }

    /**
     * We don't want the task to be recreated, because at the moment the task
     * was cancelled, the server may have already created the new dir.
     */
    @Override
    protected TaskDialog.Task onRestoreTaskState(Bundle outState) {
        return null;
    }
}