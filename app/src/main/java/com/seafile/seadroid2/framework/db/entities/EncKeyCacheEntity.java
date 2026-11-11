package com.seafile.seadroid2.framework.db.entities;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.seafile.seadroid2.framework.model.BaseModel;
import com.google.gson.annotations.SerializedName;

/**
 * v = 1: An asymmetric algorithm is used
 * v = 2: A symmetrical algorithm is used
 */
@Entity(tableName = "enc_key_cache")
public class EncKeyCacheEntity extends BaseModel {
    @SerializedName("repo_id")
    @NonNull
    @PrimaryKey
    public String repo_id = "";

    @SerializedName("enc_key")
    public String enc_key;
    @SerializedName("enc_iv")
    public String enc_iv;
    @SerializedName("enc_version")
    public int enc_version;
    @SerializedName("related_account")
    public String related_account;  //related account

    /**
     * The millisecond of the expiration timestamp.There is encryption, and there must be decryption.
     * If it is 0, it means that there is no decryption.
     */
    @SerializedName("expire_time_long")
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
