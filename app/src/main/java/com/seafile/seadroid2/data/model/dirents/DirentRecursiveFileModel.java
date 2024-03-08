package com.seafile.seadroid2.data.model.dirents;

public class DirentRecursiveFileModel {
    public String name;
    public String parent_dir;
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

}
