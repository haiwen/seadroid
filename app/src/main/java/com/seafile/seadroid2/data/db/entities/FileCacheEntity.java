package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "file_cache")
public class FileCacheEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String file_id;
    public String path;
    public String repo_id;
    public String repo_name;
    public String related_account;

    @Override
    public String toString() {
        return "FileCacheEntity{" +
                "id=" + id +
                ", file_id='" + file_id + '\'' +
                ", path='" + path + '\'' +
                ", repo_id='" + repo_id + '\'' +
                ", repo_name='" + repo_name + '\'' +
                ", related_account='" + related_account + '\'' +
                '}';
    }
}
