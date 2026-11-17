package com.seafile.seadroid2.framework.model.dirents;

import com.seafile.seadroid2.framework.model.ResultModel;
import com.google.gson.annotations.SerializedName;

public class DirentFileModel extends ResultModel {
    @SerializedName("name")
    public String name;
    //net data id(file_id/folder_id)
    //when file is empty, id is 0000000000000000000000000000000000000000
    @SerializedName("id")
    public String id;
    @SerializedName("type")
    public String type;
    @SerializedName("mtime")
    public long mtime;   // last modified timestamp
    @SerializedName("permission")
    public String permission;

    @SerializedName("size")
    public long size;    // size of file, 0 if type is dir
    @SerializedName("starred")
    public boolean starred;
    @SerializedName("can_edit")
    public boolean can_edit;
    @SerializedName("comment_total")
    public int comment_total;

    @SerializedName("lock_owner")
    public String lock_owner;

    @SerializedName("last_modified")
    public String last_modified;
    @SerializedName("last_modifier_email")
    public String last_modifier_email;
    @SerializedName("last_modifier_name")
    public String last_modifier_name;
    @SerializedName("last_modifier_contact_email")
    public String last_modifier_contact_email;

    public long getMtimeInMills() {
        return mtime * 1000;
    }

    public DirentFileModel() {
    }

    public DirentFileModel(String errMsg) {
        this.error_msg = errMsg;
    }
}
