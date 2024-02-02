package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "enc_key_cache")
public class EncKeyCacheEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String enc_key;
    public String enc_iv;
    public String repo_id;
    public String related_account;  //related account

    @Override
    public String toString() {
        return "EncKeyCacheEntity{" +
                "id=" + id +
                ", enc_key='" + enc_key + '\'' +
                ", enc_iv='" + enc_iv + '\'' +
                ", repo_id='" + repo_id + '\'' +
                '}';
    }
}
