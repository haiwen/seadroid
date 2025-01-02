package com.seafile.seadroid2.framework.data.model;

public class ContextModel {
    public String repo_id;
    public String repo_name;   //repo_name

    public String type;   //repo/dirent

    /**
     * parent_dir + name
     */
    public String full_path;
    public String permission;
}
