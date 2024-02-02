package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "repo_dir")
public class RepoDirEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String related_account;
    public String repo_id;
    public String repo_dir;
    public String related_account_email;

    @Override
    public String toString() {
        return "RepoDirEntity{" +
                "id=" + id +
                ", related_account='" + related_account + '\'' +
                ", repo_id='" + repo_id + '\'' +
                ", repo_dir='" + repo_dir + '\'' +
                '}';
    }
}
