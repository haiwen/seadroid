package com.seafile.seadroid2.transfer;

public class PendingUploadInfo {

    public String repoID;
    public String repoName;
    public String targetDir;
    public String localFilePath;
    public boolean isUpdate;

    public PendingUploadInfo(String repoID, String repoName, String targetDir,
            String localFilePath, boolean isUpdate) {
        this.repoID = repoID;
        this.repoName = repoName;
        this.targetDir = targetDir;
        this.localFilePath = localFilePath;
        this.isUpdate = isUpdate;
    }

}
