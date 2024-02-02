package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "dirents_cache")
public class DirentsCacheEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String repo_id;
    public String path;
    public String dir_id;
    public String related_account;  //related account

    @Override
    public String toString() {
        return "DirentsCacheEntity{" +
                "id=" + id +
                ", repo_id='" + repo_id + '\'' +
                ", path='" + path + '\'' +
                ", dir_id='" + dir_id + '\'' +
                '}';
    }
}
