package com.seafile.seadroid2.backupdirectory;

import com.google.common.base.Objects;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;

import java.io.Serializable;

public class UploadDirConfig implements Serializable {
    public Account account;
    public String repoID;
    public String repoName;
    public String filePath;
    public String fileName;

    public UploadDirConfig(Account account, String repoID, String repoName,
                           String fileName, String filePath) {

        this.account = account;
        this.repoID = repoID;
        this.repoName = repoName;
        this.fileName = fileName;
        this.filePath = filePath;
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

        UploadDirConfig that = (UploadDirConfig) obj;
        if(that.account == null || that.repoID == null || that.repoName == null || that.filePath == null) {
            return false;
        }

        return that.account.equals(this.account) && that.repoID.equals(this.repoID) &&
                that.repoName.equals(this.repoName) && that.filePath.equals(this.filePath);
    }

    private volatile int hashCode = 0;

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = Objects.hashCode(account, repoID, repoName, filePath);
        }

        return hashCode;
    }
}
