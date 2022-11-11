package com.seafile.seadroid2.backupdirectory;

import com.google.common.base.Objects;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;

import java.io.Serializable;

public class UploadDirInfo implements Serializable {
    public Account account;
    public String repoID;
    public String repoName;
    public String parentDir;
    public String filePath;
    public String fileName;
    public String fileSize;

    public UploadDirInfo(Account account, String repoID, String repoName, String parentDir,
                         String fileName, String filePath, String fileSize) {

        this.account = account;
        this.repoID = repoID;
        this.repoName = repoName;
        this.parentDir = parentDir;
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

        UploadDirInfo that = (UploadDirInfo) obj;
        if(that.account == null || that.repoID == null || that.repoName == null || that.parentDir == null || that.filePath == null) {
            return false;
        }

        return that.account.equals(this.account) && that.repoID.equals(this.repoID) &&
                that.repoName.equals(this.repoName) && that.parentDir.equals(this.parentDir) &&
                that.filePath.equals(this.filePath);
    }

    private volatile int hashCode = 0;

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = Objects.hashCode(account, repoID, repoName, parentDir, filePath);
        }

        return hashCode;
    }
}
