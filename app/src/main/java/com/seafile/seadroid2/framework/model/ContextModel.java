package com.seafile.seadroid2.framework.model;

import com.google.gson.annotations.SerializedName;

public class ContextModel {
    //account
    @SerializedName("email")
    public String email;
    @SerializedName("server")
    public String server;
    @SerializedName("name")
    public String name;


    //repo
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("repo_name")
    public String repo_name;

    @SerializedName("type")
    public String type;   // account/repo/dirent

    /**
     * parent_dir + name
     */
    @SerializedName("full_path")
    public String full_path;
    @SerializedName("permission")
    public String permission;
    @SerializedName("encrypted")
    public boolean encrypted;
}
