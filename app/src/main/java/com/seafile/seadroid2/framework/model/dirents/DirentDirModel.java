package com.seafile.seadroid2.framework.model.dirents;

import com.seafile.seadroid2.framework.model.ResultModel;
import com.google.gson.annotations.SerializedName;

public class DirentDirModel extends ResultModel {
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("path")
    public String path;
    @SerializedName("name")
    public String name;
    @SerializedName("mtime")
    public long mtime;
    @SerializedName("permission")
    public String permission;
}
