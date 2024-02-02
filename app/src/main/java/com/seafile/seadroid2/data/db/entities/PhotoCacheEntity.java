package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "photo_cache")
public class PhotoCacheEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String file;
    public long date_added;
    public String related_account;


    @Override
    public String toString() {
        return "PhotoCacheEntity{" +
                "id=" + id +
                ", file='" + file + '\'' +
                ", date_added=" + date_added +
                '}';
    }
}
