package com.seafile.seadroid2.framework.db.entities;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.seafile.seadroid2.framework.model.BaseModel;

/**
 * v = 1: An asymmetric algorithm is used
 * v = 2: A symmetrical algorithm is used
 */
@Entity(tableName = "enc_key_cache")
public class EncKeyCacheEntity extends BaseModel {
    @NonNull
    @PrimaryKey
    public String repo_id = "";

    public String enc_key;
    public String enc_iv;
    public int enc_version;
    public String related_account;  //related account

    /**
     * The millisecond of the expiration timestamp.There is encryption, and there must be decryption.
     * If it is 0, it means that there is no decryption.
     */
    public long expire_time_long;

    @NonNull
    @Override
    public String toString() {
        return "EncKeyCacheEntity{" +
                ", repo_id='" + repo_id + '\'' +
                ", enc_key='" + enc_key + '\'' +
                ", expire_time_long='" + expire_time_long + '\'' +
                '}';
    }
}
