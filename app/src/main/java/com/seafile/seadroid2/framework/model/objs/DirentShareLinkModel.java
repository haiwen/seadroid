package com.seafile.seadroid2.framework.model.objs;

import com.seafile.seadroid2.framework.model.dirents.DirentPermissionModel;
import com.google.gson.annotations.SerializedName;

public class DirentShareLinkModel {
    @SerializedName("username")
    public String username;
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("repo_name")
    public String repo_name;
    @SerializedName("path")
    public String path;
    @SerializedName("password")
    public String password;
    @SerializedName("obj_name")
    public String obj_name;
    @SerializedName("is_dir")
    public boolean is_dir;
    @SerializedName("token")
    public String token;
    @SerializedName("link")
    public String link;
    @SerializedName("view_count")
    public int view_count;
    @SerializedName("ctime")
    public String ctime;
    @SerializedName("expire_date")
    public String expire_date;
    @SerializedName("is_expired")
    public boolean is_expired;
    @SerializedName("permissions")
    public DirentPermissionModel permissions;

}
