package com.seafile.seadroid2.transfer;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.transfer.TransferManager.TaskState;

public class UploadTaskInfo {
    public final int taskID;
    public final TaskState state;
    public final String repoID;
    public final String repoName;
    public final String parentDir;
    public final String localFilePath;
    public final boolean isUpdate, isCopyToLocal;
    public final long uploadedSize, totalSize;
    public final SeafException err;
    public final Account account;

    public UploadTaskInfo(int taskID, Account account, TaskState state, String repoID,
                          String repoName, String parentDir,
                          String localFilePath, boolean isUpdate, boolean isCopyToLocal,
                          long uploadedSize, long totalSize,
                          SeafException err) {
        this.taskID = taskID;
        this.account = account;
        this.state = state;
        this.repoID = repoID;
        this.repoName = repoName;
        this.parentDir = parentDir;
        this.localFilePath = localFilePath;
        this.isUpdate = isUpdate;
        this.isCopyToLocal = isCopyToLocal;
        this.uploadedSize = uploadedSize;
        this.totalSize = totalSize;
        this.err = err;
    }
}
