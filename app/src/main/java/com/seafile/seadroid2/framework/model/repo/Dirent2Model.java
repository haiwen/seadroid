package com.seafile.seadroid2.framework.model.repo;

import com.seafile.seadroid2.framework.model.BaseModel;
import com.google.gson.annotations.SerializedName;

public class Dirent2Model extends BaseModel {
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("repo_name")
    public String repo_name;
    @SerializedName("path")
    public String path;
    @SerializedName("obj_name")
    public String obj_name;
    @SerializedName("mtime")
    public String mtime;
    @SerializedName("user_email")
    public String user_email;
    @SerializedName("user_name")
    public String user_name;
    @SerializedName("user_contact_email")
    public String user_contact_email;
    @SerializedName("repo_encrypted")
    public boolean repo_encrypted;
    @SerializedName("is_dir")
    public boolean is_dir;

    @Override
    public String toString() {
        return "Dirent2Model{" +
                "repo_id='" + repo_id + '\'' +
                ", repo_name='" + repo_name + '\'' +
                ", path='" + path + '\'' +
                ", obj_name='" + obj_name + '\'' +
                ", mtime='" + mtime + '\'' +
                ", user_email='" + user_email + '\'' +
                ", user_name='" + user_name + '\'' +
                ", user_contact_email='" + user_contact_email + '\'' +
                ", repo_encrypted=" + repo_encrypted +
                ", is_dir=" + is_dir +
                '}';
    }
}
