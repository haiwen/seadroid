package com.seafile.seadroid2.folderbackup;

import com.google.common.base.Objects;
import com.seafile.seadroid2.SettingsManager;

import java.io.Serializable;

public class RepoConfig implements Serializable {
    private String email;

    private String repoID;

    private String repoName;

    public RepoConfig(String repoID, String repoName, String email) {
        this.repoID=repoID;
        this.repoName=repoName;
        this.email=email;
    }


    public void setEmail(String email) {
        this.email = email;
    }

    public String getEmail() {
        return email;
    }

    public void setRepoID(String repoID) {
        this.repoID = repoID;
    }

    public String getRepoID() {
        return repoID;
    }

    public void setRepoName(String repoName) {
        this.repoName = repoName;
    }

    public String getRepoName() {
        return repoName;
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

        RepoConfig that = (RepoConfig) obj;
        if(that.repoID == null || that.repoName == null || that.email == null) {
            return false;
        }

        return that.email.equals(this.email) && that.repoID.equals(this.repoID) &&
                that.repoName.equals(this.repoName) ;
    }

    private volatile int hashCode = 0;

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = Objects.hashCode(email, repoID, repoName);
        }

        return hashCode;
    }
}
