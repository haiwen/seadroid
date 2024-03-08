package com.seafile.seadroid2.data.db.entities;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "objs")
public class ObjsModel {

    /**
     * when type is repo, this path is repo id.
     * when type is dirent(dir/file), this path is full_path, like this: /a/b/ or /a/b/c/d.md
     */
    @PrimaryKey
    @NonNull
    public String path = "";

    //repo/dir/file
    public String type;

//    public long refresh_time_long;

    // The millisecond of the expiration timestamp.
    // There is encryption, and there must be decryption.
    // If it is 0, it means that there is no decryption.
    public long decrypt_expire_time_long;

    public String related_account;  //related account
}
