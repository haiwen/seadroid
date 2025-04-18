package com.seafile.seadroid2.framework.data.model.dirents;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.util.Utils;


public class DirentRecursiveFileModel {
    public String name;
    public String parent_dir;

    public String getParent_dir() {
        if (TextUtils.isEmpty(parent_dir)) {
            parent_dir = "/";
        }

        if (!parent_dir.endsWith("/")) {
            parent_dir += "/";
        }

        return parent_dir;
    }

    //net data id(file_id/folder_id)
    //when file is empty, id is 0000000000000000000000000000000000000000
    public String id;
    public String type;
    public long mtime;   // last modified timestamp
    public String permission;

    public long size;    // size of file, 0 if type is dir

    //lock
    public boolean is_locked;

    public boolean locked_by_me;
    public long lock_time;
    public String lock_owner;

    public String modifier_name;
    public String modifier_contact_email;

    public boolean isDir() {
        return TextUtils.equals(type, "dir");
    }

    public String getFullFileName() {
        return Utils.pathJoin(parent_dir, name);
    }


}
