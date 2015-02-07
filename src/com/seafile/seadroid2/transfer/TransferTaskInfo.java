package com.seafile.seadroid2.transfer;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;

/**
 * Base class
 * <p/>
 * reference for override equals and hashcode, http://www.javaranch.com/journal/2002/10/equalhash.html
 */
public class TransferTaskInfo {
    public final Account account;
    public final int taskID;
    public final TaskState state;
    public final String repoID;
    public final String repoName;
    public final String localFilePath;
    public final SeafException err;

    /**
     * Constructor
     *
     * @param account   Current login Account instance
     * @param taskID    TransferTask id
     * @param state     TransferTask state, value is one of INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED of {@link TaskState}
     * @param repoID    Repository id
     * @param repoName  Repository name
     * @param localPath Local path
     * @param err       Exception instance of {@link SeafException}
     */
    public TransferTaskInfo(Account account, int taskID, TaskState state, String repoID,
                            String repoName, String localPath,
                            SeafException err) {
        this.account = account;
        this.taskID = taskID;
        this.state = state;
        this.repoID = repoID;
        this.repoName = repoName;
        this.localFilePath = localPath;
        this.err = err;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if ((obj == null) || (obj.getClass() != this.getClass()))
            return false;
        TransferTaskInfo tti = (TransferTaskInfo) obj;
        return (account.getSignature() == tti.account.getSignature() || (account.getSignature() != null && account.getSignature().equals(tti.account.getSignature())))
                && (repoID == tti.repoID || (repoID != null && repoID.equals(tti.repoID)))
                && (localFilePath == tti.localFilePath || (localFilePath != null && localFilePath.equals(tti.localFilePath)));
    }

    @Override
    public String toString() {
        return "email " + account.getEmail() + " server " + account.getServer() + " taskID " + taskID + " repoID " + repoID +
                " repoName " + repoName + " localFilePath " + localFilePath;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + (account.getSignature() == null ? 0 : account.getSignature().hashCode());
        hash = 31 * hash + (repoID == null ? 0 : repoID.hashCode());
        hash = 31 * hash + (localFilePath == null ? 0 : localFilePath.hashCode());
        return hash;
    }
}
