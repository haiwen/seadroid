package com.seafile.seadroid2.data.db.entities;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "objs")
public class ObjsModel {

    /**
     * when type is repo, this path is repo id.
     * when type is dir/file, this path is full_path, like this: /1/2 or /1/2/3/4.md
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


    public String related_account_email;  //related account
}
