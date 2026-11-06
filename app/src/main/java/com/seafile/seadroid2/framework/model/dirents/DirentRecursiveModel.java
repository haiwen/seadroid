package com.seafile.seadroid2.framework.model.dirents;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.util.Utils;
import com.google.gson.annotations.SerializedName;


public class DirentRecursiveModel {
    @SerializedName("name")
    public String name;
    @SerializedName("parent_dir")
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

    //lock
    @SerializedName("is_locked")
    public boolean is_locked;

    @SerializedName("locked_by_me")
    public boolean locked_by_me;
    @SerializedName("lock_time")
    public long lock_time;
    @SerializedName("lock_owner")
    public String lock_owner;

    @SerializedName("modifier_name")
    public String modifier_name;
    @SerializedName("modifier_email")
    public String modifier_email;
    @SerializedName("modifier_contact_email")
    public String modifier_contact_email;

    public boolean isDir() {
        return TextUtils.equals(type, "dir");
    }

    public String getFullFileName() {
        return Utils.pathJoin(parent_dir, name);
    }

    @Nullable
    public static DirentRecursiveModel convertDirentModelToThis(DirentModel direntModel) {
        if (direntModel == null) {
            return null;
        }

        DirentRecursiveModel fileModel = new DirentRecursiveModel();
        fileModel.name = direntModel.name;
        fileModel.parent_dir = direntModel.parent_dir;
        fileModel.id = direntModel.id;
        fileModel.type = direntModel.type;
        fileModel.mtime = direntModel.mtime;
        fileModel.permission = direntModel.permission;
        fileModel.size = direntModel.size;
        fileModel.is_locked = direntModel.is_locked;
        fileModel.locked_by_me = direntModel.locked_by_me;
        fileModel.lock_time = direntModel.lock_time;
        fileModel.lock_owner = direntModel.lock_owner;
        fileModel.modifier_name = direntModel.modifier_name;
        fileModel.modifier_email = direntModel.modifier_email;
        fileModel.modifier_contact_email = direntModel.modifier_contact_email;
        return fileModel;
    }

}
