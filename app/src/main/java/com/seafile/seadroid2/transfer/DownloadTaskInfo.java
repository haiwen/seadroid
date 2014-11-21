package com.seafile.seadroid2.transfer;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.transfer.TransferManager.TaskState;

public class DownloadTaskInfo {
    public final Account account;
    public final int taskID;
    public final TaskState state;
    public final String repoID;
    public final String repoName;
    public final String pathInRepo;
    public final String localFilePath;
    public final long fileSize, finished;
    public final SeafException err;

    public DownloadTaskInfo(Account account, int taskID, TaskState state, String repoID,
                            String repoName, String path, String localPath,
                            long fileSize, long finished,
                            SeafException err) {
        this.account = account;
        this.taskID = taskID;
        this.state = state;
        this.repoID = repoID;
        this.repoName = repoName;
        this.pathInRepo = path;
        this.localFilePath = localPath;
        this.fileSize = fileSize;
        this.finished = finished;
        this.err = err;
    }
}
