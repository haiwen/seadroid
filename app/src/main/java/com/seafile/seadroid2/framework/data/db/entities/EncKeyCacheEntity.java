package com.seafile.seadroid2.framework.data.db.entities;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.seafile.seadroid2.framework.data.model.BaseModel;

@Entity(tableName = "enc_key_cache")
public class EncKeyCacheEntity extends BaseModel {

    @NonNull
    @PrimaryKey
    public String repo_id;

    public String enc_key;
    public String enc_iv;
    public int enc_version;
    public String related_account;  //related account


    // The millisecond of the expiration timestamp.
    // There is encryption, and there must be decryption.
    // If it is 0, it means that there is no decryption.
    public long expire_time_long;

    @Override
    public String toString() {
        return "EncKeyCacheEntity{" +
                ", enc_key='" + enc_key + '\'' +
                ", enc_iv='" + enc_iv + '\'' +
                ", repo_id='" + repo_id + '\'' +
                ", expire_time_long='" + expire_time_long + '\'' +
                '}';
    }
}
