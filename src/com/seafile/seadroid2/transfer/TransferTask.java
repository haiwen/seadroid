package com.seafile.seadroid2.transfer;

import android.os.AsyncTask;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;

import java.io.File;

/**
 * Base class for transferring data
 * <p/>
 * reference for override equals and hashcode, http://www.javaranch.com/journal/2002/10/equalhash.html
 * <p/>
 */
public abstract class TransferTask extends AsyncTask<Void, Long, File> {

    protected int taskID;
    protected Account account;
    protected String repoName;
    protected String repoID;
    protected String path;
    protected long totalSize, finished;
    protected TaskState state;
    protected SeafException err;

    public TransferTask(int taskID, Account account, String repoName, String repoID, String path) {
        this.account = account;
        this.repoName = repoName;
        this.repoID = repoID;
        this.path = path;
        this.state = TaskState.INIT;

        // The size of the file would be known in the first progress update
        this.totalSize = -1;
        this.taskID = taskID;
    }

    protected void cancel() {
        if (state != TaskState.INIT && state != TaskState.TRANSFERRING) {
            return;
        }
        state = TaskState.CANCELLED;
        super.cancel(true);
    }

    protected boolean canRetry() {
        return state == TaskState.CANCELLED || state == TaskState.FAILED;
    }

    protected abstract TransferTaskInfo getTaskInfo();

    public int getTaskID() {
        return taskID;
    }

    public TaskState getState() {
        return state;
    }

    public Account getAccount() {
        return account;
    }

    public String getRepoName() {
        return repoName;
    }

    public String getRepoID() {
        return repoID;
    }

    public long getTotalSize() {
        return totalSize;
    }

    public long getFinished() {
        return finished;
    }

    public String getPath() {
        return path;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if ((obj == null) || (obj.getClass() != this.getClass()))
            return false;
        TransferTask tt = (TransferTask) obj;
        return (account.getSignature() == tt.account.getSignature() || (account.getSignature() != null && account.getSignature().equals(tt.account.getSignature())))
                && (repoID == tt.repoID || (repoID != null && repoID.equals(tt.repoID)))
                && (path == tt.path || (path != null && path.equals(tt.path)));
    }

    @Override
    public String toString() {
        return "email " + account.getEmail() + " server " + account.getServer() + " taskID " + taskID + " repoID " + repoID +
                " repoName " + repoName + " path " + path;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + (account.getSignature() == null ? 0 : account.getSignature().hashCode());
        hash = 31 * hash + (repoID == null ? 0 : repoID.hashCode());
        hash = 31 * hash + (path == null ? 0 : path.hashCode());
        return hash;
    }
}
