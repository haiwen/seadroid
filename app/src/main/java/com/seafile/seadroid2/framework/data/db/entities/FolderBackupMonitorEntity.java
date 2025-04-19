package com.seafile.seadroid2.framework.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Deprecated
@Entity(tableName = "folder_monitor_cache")
public class FolderBackupMonitorEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String repo_id;
    public String repo_name;
    public String related_account;
    public String parent_dir;
    public String local_path;
    public String version;

    @Override
    public String toString() {
        return "MonitorEntity{" +
                "id=" + id +
                ", repo_id='" + repo_id + '\'' +
                ", repo_name='" + repo_name + '\'' +
                ", related_account='" + related_account + '\'' +
                ", parent_dir='" + parent_dir + '\'' +
                ", local_path='" + local_path + '\'' +
                ", version='" + version + '\'' +
                '}';

    }
}
