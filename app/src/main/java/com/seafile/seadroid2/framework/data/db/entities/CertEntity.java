package com.seafile.seadroid2.framework.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Deprecated
@Entity(tableName = "cert_cache")
public class CertEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String url;
    public String cert;

    @Override
    public String toString() {
        return "CertEntity{" +
                "url='" + url + '\'' +
                ", cert='" + cert + '\'' +
                '}';
    }
}
