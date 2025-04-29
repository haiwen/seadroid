package com.seafile.seadroid2.framework.model.repo;

import com.seafile.seadroid2.framework.model.BaseModel;

public class Dirent2Model extends BaseModel {
    public String repo_id;
    public String repo_name;
    public String path;
    public String obj_name;
    public String mtime;
    public String user_email;
    public String user_name;
    public String user_contact_email;
    public boolean repo_encrypted;
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
