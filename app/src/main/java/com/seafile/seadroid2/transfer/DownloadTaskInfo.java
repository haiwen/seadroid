package com.seafile.seadroid2.transfer;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
/**
 *  download task info
 */
public class DownloadTaskInfo extends TransferTaskInfo {

    public final String pathInRepo;
    public final long fileSize, finished;

    /**
     * Constructor of DownloadTaskInfo
     *  @param account Current login Account instance
     * @param taskID TransferTask id
     * @param state TransferTask state, value is one of <strong>INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED</strong> of {@link TaskState}
     * @param repoID Repository id
     * @param repoName Repository name
     * @param pathInRepo File path in Repository
     * @param localPath Local path
     * @param fileSize File total size
     * @param finished File downloaded size
     * @param err Exception instance of {@link SeafException}
     */
    public DownloadTaskInfo(Account account, int taskID, TaskState state,
                            String repoID, String repoName, String pathInRepo,
                            String localPath, long fileSize, long finished, SeafException err) {
        super(account, taskID, state, repoID, repoName, localPath, err);

        this.pathInRepo = pathInRepo;
        this.fileSize = fileSize;
        this.finished = finished;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;

        DownloadTaskInfo a = (DownloadTaskInfo) obj;
        if (!super.equals(a))
            return false;

        if (a.pathInRepo == null)
            return false;

        return a.pathInRepo.equals(this.pathInRepo) && a.fileSize == this.fileSize && a.finished == this.finished;
    }
}
