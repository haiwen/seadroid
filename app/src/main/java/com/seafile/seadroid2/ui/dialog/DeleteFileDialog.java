package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;

import java.util.List;

/**
 * * AsyncTask for deleting files
 */
class DeleteTask extends TaskDialog.Task {
    public static final String DEBUG_TAG = "DeleteTask";

    String repoID;
    List<SeafDirent> dirents;
    String path;
    boolean isdir;
    DataManager dataManager;
    DeleteTaskManager manager;

    public DeleteTask(String repoID, String path, boolean isdir, DataManager dataManager) {
        this.repoID = repoID;
        this.path = path;
        this.isdir = isdir;
        this.dataManager = dataManager;
    }

    public DeleteTask(String repoID, String path, List<SeafDirent> dirents, DataManager dataManager) {
        this.repoID = repoID;
        this.path = path;
        this.dirents = dirents;
        this.dataManager = dataManager;
        this.manager = new DeleteTaskManager();
    }

    @Override
    protected void runTask() {
        try {
            // batch operation
            if (dirents != null) {
                for (SeafDirent dirent : dirents) {
                    DeleteCell cell = new DeleteCell(repoID, path + "/" + dirent.name, dirent.isDir());
                    manager.addTaskToQue(cell);
                }
                manager.doNext();
            } else
                dataManager.delete(repoID, path, isdir);
        } catch (SeafException e) {
            setTaskException(e);
        }
    }

    /**
     * Class for deleting files sequentially, starting one after the previous completes.
     */
    class DeleteTaskManager {

        protected List<DeleteCell> waitingList = Lists.newArrayList();

        private synchronized boolean hasInQue(DeleteCell deleteTask) {
            if (waitingList.contains(deleteTask)) {
                // Log.d(DEBUG_TAG, "in  Que  " + deleteTask.getPath() + "in waiting list");
                return true;
            }

            return false;
        }

        public void addTaskToQue(DeleteCell cell) {
            if (!hasInQue(cell)) {
                // remove the cancelled or failed cell if any
                synchronized (this) {
                    // Log.d(DEBUG_TAG, "------ add Que  " + cell.getPath());
                    waitingList.add(cell);
                }
            }
        }

        public synchronized void doNext() {
            if (!waitingList.isEmpty()) {
                // Log.d(DEBUG_TAG, "--- do next!");

                DeleteCell cell = waitingList.remove(0);

                try {
                    dataManager.delete(cell.getRepoID(), cell.getPath(), cell.isdir);
                } catch (SeafException e) {
                    setTaskException(e);
                }
                doNext();
            }
        }

    }

    /**
     * Class for queuing deleting tasks
     */
    class DeleteCell {
        private String repoID;
        private String path;
        private boolean isdir;

        public DeleteCell(String repoID, String path, boolean isdir) {
            this.repoID = repoID;
            this.path = path;
            this.isdir = isdir;
        }

        public String getRepoID() {
            return repoID;
        }

        public String getPath() {
            return path;
        }

        public boolean isdir() {
            return isdir;
        }
    }
}

public class DeleteFileDialog extends TaskDialog {
    private String repoID;
    private String path;
    private List<SeafDirent> dirents;
    private boolean isdir;

    private DataManager dataManager;
    private Account account;

    public void init(String repoID, String path, boolean isdir, Account account) {
        this.repoID = repoID;
        this.path = path;
        this.isdir = isdir;
        this.account = account;
    }

    public void init(String repoID, String path, List<SeafDirent> dirents, Account account) {
        this.repoID = repoID;
        this.path = path;
        this.dirents = dirents;
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
        dialog.setTitle(str);
    }

    @Override
    protected DeleteTask prepareTask() {
        if (dirents != null) {
            return new DeleteTask(repoID, path, dirents, getDataManager());
        }
        return new DeleteTask(repoID, path, isdir, getDataManager());
    }
}
