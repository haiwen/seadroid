package com.seafile.seadroid2.transfer;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
/**
 * upload task info
 */
public class UploadTaskInfo extends TransferTaskInfo {

    public final String parentDir;
    public final long uploadedSize, totalSize;
    public final boolean isUpdate, isCopyToLocal;
    public int version;

    /**
     * Constructor of UploadTaskInfo
     *  @param account Current login Account instance
     * @param taskID TransferTask id
     * @param state TransferTask state, value is one of <strong>INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED</strong> of {@link TaskState}
     * @param repoID Repository id
     * @param repoName Repository name
     * @param parentDir Parent directory of the file
     * @param localPath Local path
     * @param isUpdate Force to update files if true
     * @param isCopyToLocal Copy files to SD card if true
     * @param uploadedSize File uploaded size
     * @param totalSize File total size
     * @param err Exception instance of {@link SeafException}
     */
    public UploadTaskInfo(Account account,
                          int taskID,
                          TaskState state,
                          String repoID,
                          String repoName,
                          String parentDir,
                          String localPath,
                          boolean isUpdate,
                          boolean isCopyToLocal,
                          long uploadedSize,
                          long totalSize,
                          SeafException err) {

        super(account, taskID, state, repoID, repoName, localPath, err);

        this.parentDir = parentDir;
        this.uploadedSize = uploadedSize;
        this.totalSize = totalSize;
        this.isUpdate = isUpdate;
        this.isCopyToLocal = isCopyToLocal;
    }
}
