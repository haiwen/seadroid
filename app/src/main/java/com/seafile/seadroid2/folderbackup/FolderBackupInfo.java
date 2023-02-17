package com.seafile.seadroid2.folderbackup;

import com.google.common.base.Objects;
import com.seafile.seadroid2.SettingsManager;

import java.io.Serializable;

public class FolderBackupInfo implements Serializable {
    private volatile int hashCode = 0;
    public String repoID;
    public String repoName;
    public String parentFolder;
    public String filePath;
    public String fileName;
    public String fileSize;

    public FolderBackupInfo(String repoID, String repoName, String parentPath,
                            String fileName, String filePath, String fileSize) {
        this.repoID = repoID;
        this.repoName = repoName;
        this.parentFolder = parentPath;
        this.fileName = fileName;
        this.filePath = filePath;
        this.fileSize = fileSize;
    }

    public boolean canLocalDecrypt() {
        return SettingsManager.instance().isEncryptEnabled();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;

        FolderBackupInfo that = (FolderBackupInfo) obj;
        if (that.repoID == null || that.repoName == null ||
                that.parentFolder == null || that.filePath == null) {
            return false;
        }
        return that.repoID.equals(this.repoID) &&
                that.repoName.equals(this.repoName) &&
                that.parentFolder.equals(this.parentFolder) &&
                that.filePath.equals(this.filePath);
    }

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = Objects.hashCode(repoID, repoName, parentFolder, filePath);
        }
        return hashCode;
    }
}
