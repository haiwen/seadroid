package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "starred_file_cache")
public class StarredFileCacheEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String content;
    public String related_account;

    @Override
    public String toString() {
        return "StarredFileCacheEntity{" +
                "id=" + id +
                ", content='" + content + '\'' +
                ", related_account='" + related_account + '\'' +
                '}';
    }
}
