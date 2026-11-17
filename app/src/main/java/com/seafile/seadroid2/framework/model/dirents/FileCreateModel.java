package com.seafile.seadroid2.framework.model.dirents;

import com.google.gson.annotations.SerializedName;

public class FileCreateModel {
    @SerializedName("error_msg")
    public String error_msg;
    @SerializedName("can_edit")
    public boolean can_edit;
    @SerializedName("can_preview")
    public boolean can_preview;
    @SerializedName("is_locked")
    public boolean is_locked;
    @SerializedName("mtime")
    public String mtime;
    @SerializedName("obj_id")
    public String obj_id;
    @SerializedName("obj_name")
    public String obj_name;
    @SerializedName("parent_dir")
    public String parent_dir;
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("size")
    public long size;
    @SerializedName("type")
    public String type;

}
