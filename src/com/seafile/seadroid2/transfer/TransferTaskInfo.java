package com.seafile.seadroid2.transfer;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.transfer.TransferManager.TaskState;

/*
 *  base class for {@link DownloadTaskInfo} and {@link UploadTaskInfo}
 */
public class TransferTaskInfo{
    public final Account account;
    public final int taskID;
    public final TaskState state;
    public final String repoID;
    public final String repoName;
    public final String localFilePath;
    public final SeafException err;

    /**
     * Construct a Transfer Task Info instance
     *
     * @param account Current login Account instance
     * @param taskID TransferTask id
     * @param state TransferTask state, value is one of INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED of {@link TaskState}
     * @param repoID Repository id
     * @param repoName Repository name
     * @param localPath Local path
     * @param err Exception instance of {@link SeafException}
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
        if (!(obj instanceof TransferTaskInfo))
            return false;
        if (obj == this)
            return true;

        TransferTaskInfo tti = (TransferTaskInfo) obj;
        return tti.account.getSignature().equals(account.getSignature())
                && Integer.compare(tti.taskID, taskID) == 0
                && tti.repoID.equals(repoID);
    }

    @Override
    public String toString() {
        return "email " + account.getEmail() + " server " + account.getServer() + " taskID " + taskID + " repoID " + repoID +
                " repoName " + repoName + " localFilePath " + localFilePath;
    }

    @Override
    public int hashCode() {
        return String.format("%s%s%d", taskID, repoName, localFilePath).hashCode();
    }
}
