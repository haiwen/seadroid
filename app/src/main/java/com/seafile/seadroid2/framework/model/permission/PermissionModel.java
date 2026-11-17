package com.seafile.seadroid2.framework.model.permission;

import com.google.gson.annotations.SerializedName;

public class PermissionModel {
    @SerializedName("create")
    public boolean create;
    @SerializedName("upload")
    public boolean upload;
    @SerializedName("download")
    public boolean download;
    @SerializedName("preview")
    public boolean preview;
    @SerializedName("copy")
    public boolean copy;
    @SerializedName("delete")
    public boolean delete;
    @SerializedName("modify")
    public boolean modify;
    @SerializedName("download_external_link")
    public boolean download_external_link;
}
