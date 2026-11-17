package com.seafile.seadroid2.framework.model.dirents;

import com.google.gson.annotations.SerializedName;

public class DirentPermissionModel {
    @SerializedName("can_edit")
    public boolean can_edit;
    @SerializedName("can_download")
    public boolean can_download;
    @SerializedName("can_upload")
    public boolean can_upload;
}
