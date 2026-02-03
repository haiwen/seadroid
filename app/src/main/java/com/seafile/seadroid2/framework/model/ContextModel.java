package com.seafile.seadroid2.framework.model;

public class ContextModel {
    //account
    public String email;
    public String server;
    public String name;


    //repo
    public String repo_id;
    public String repo_name;

    public String type;   // account/repo/dirent

    /**
     * parent_dir + name
     */
    public String full_path;
    public String permission;
    public boolean encrypted;
}
