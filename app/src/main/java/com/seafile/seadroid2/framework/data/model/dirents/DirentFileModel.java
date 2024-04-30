package com.seafile.seadroid2.framework.data.model.dirents;

import com.seafile.seadroid2.framework.data.model.ResultModel;

public class DirentFileModel extends ResultModel {
    public String name;
    //net data id(file_id/folder_id)
    //when file is empty, id is 0000000000000000000000000000000000000000
    public String id;
    public String type;
    public long mtime;   // last modified timestamp
    public String permission;

    public long size;    // size of file, 0 if type is dir
    public boolean starred;
    public boolean can_edit;
    public int comment_total;

    public String lock_owner;

    public String last_modified;
    public String last_modifier_email;
    public String last_modifier_name;
    public String last_modifier_contact_email;

    public long getMtimeInMills() {
        return mtime * 1000;
    }
}
