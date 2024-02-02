package com.seafile.seadroid2.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.util.Utils;

@Entity(tableName = "dirents")
public class DirentModel extends BaseModel {

    // parent_dir + name
    // /test.txt
    // /Download
    // /Download/test.txt
    // /Download/test/test1.txt
    @PrimaryKey
    @NonNull
    public String hash_path = "";
    public String full_path = "";

    public String name;
    public String parent_dir;
    //net data id
    public String id;
    public String type;
    public long mtime;   // last modified timestamp
    public String permission;
    public String dir_id;
    public boolean starred;

    public long size;    // size of file, 0 if type is dir

    public String related_account_email;  //related account
    public String repo_id;
    public String repo_name;

    //lock
    public boolean is_locked;
    public boolean is_freezed;
    public boolean locked_by_me;
    public long lock_time;
    public String lock_owner;
    public String lock_owner_name;
    public String lock_owner_contact_email;
    public String modifier_email;
    public String modifier_name;
    public String modifier_contact_email;
    public String encoded_thumbnail_src;

    //last sync time
    public long last_sync_time = 0;

    @Ignore
    private String timestamp;

    public boolean isDir() {
        return TextUtils.equals(type, "dir");
    }

    public String getSubtitle() {
        if (TextUtils.isEmpty(timestamp)) {
            timestamp = Utils.translateCommitTime(mtime * 1000);
        }
        if (isDir())
            return timestamp;
        return Utils.readableFileSize(size) + ", " + timestamp;
    }

    public int getIcon() {
        if (isDir()) {
            if (!hasWritePermission()) {
                return R.drawable.folder_read_only;
            } else {
                return R.drawable.folder;
            }
        }
        return Utils.getFileIcon(name);
    }

    public boolean hasWritePermission() {
        return !TextUtils.isEmpty(permission) && permission.contains("w");
    }

}
