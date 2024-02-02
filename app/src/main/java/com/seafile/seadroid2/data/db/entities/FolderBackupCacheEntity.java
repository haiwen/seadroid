package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "folder_backup_cache")
public class FolderBackupCacheEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String repo_id;
    public String repo_name;
    public String parent_folder;
    public String file_name;
    public String file_path;
    public long file_size;
    public String related_account;

    @Override
    public String toString() {
        return "FolderBackupCacheEntity{" +
                "id=" + id +
                ", repo_id='" + repo_id + '\'' +
                ", repo_name='" + repo_name + '\'' +
                ", parent_folder='" + parent_folder + '\'' +
                ", file_name='" + file_name + '\'' +
                ", file_path='" + file_path + '\'' +
                ", file_size=" + file_size +
                '}';
    }
}
