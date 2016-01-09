package com.seafile.seadroid2.transfer;

public class PendingUploadInfo {

    public String repoID;
    public String repoName;
    public String targetDir;
    public String localFilePath;
    public boolean isUpdate;
    public boolean isCopyToLocal;

    public PendingUploadInfo(String repoID, String repoName, String targetDir,
            String localFilePath, boolean isUpdate, boolean isCopyToLocal) {
        this.repoID = repoID;
        this.repoName = repoName;
        this.targetDir = targetDir;
        this.localFilePath = localFilePath;
        this.isUpdate = isUpdate;
        this.isCopyToLocal = isCopyToLocal;
    }

}
