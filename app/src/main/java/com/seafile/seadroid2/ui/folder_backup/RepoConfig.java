package com.seafile.seadroid2.ui.folder_backup;

import com.google.common.base.Objects;
import com.google.gson.annotations.SerializedName;

import java.io.Serializable;

public class RepoConfig implements Serializable {
    @SerializedName("email")
    private String email;
    @SerializedName("signature")
    private String signature;

    @SerializedName("repoID")
    private String repoID;
    @SerializedName("repoName")
    private String repoName;

    public RepoConfig(String repoID, String repoName, String email,String signature) {
        this.repoID = repoID;
        this.repoName = repoName;
        this.email = email;
        this.signature = signature;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getEmail() {
        return email;
    }

    public void setRepoId(String repoID) {
        this.repoID = repoID;
    }

    public String getRepoId() {
        return repoID;
    }

    public void setRepoName(String repoName) {
        this.repoName = repoName;
    }

    public String getRepoName() {
        return repoName;
    }

    public String getSignature() {
        return signature;
    }

    public void setSignature(String signature) {
        this.signature = signature;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;

        RepoConfig that = (RepoConfig) obj;
        if (that.repoID == null || that.repoName == null || that.email == null) {
            return false;
        }

        return that.email.equals(this.email) && that.repoID.equals(this.repoID) &&
                that.repoName.equals(this.repoName);
    }

    @SerializedName("hashCode")
    private volatile int hashCode = 0;

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = Objects.hashCode(email, repoID, repoName);
        }
        return hashCode;
    }

    @Override
    public String toString() {
        return "RepoConfig{" +
                "email='" + email + '\'' +
                ", signature='" + signature + '\'' +
                ", repoID='" + repoID + '\'' +
                ", repoName='" + repoName + '\'' +
                ", hashCode=" + hashCode +
                '}';
    }
}
