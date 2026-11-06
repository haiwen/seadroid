package com.seafile.seadroid2.framework.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;
import com.google.gson.annotations.SerializedName;

@Deprecated
@Entity(tableName = "folder_monitor_cache")
public class FolderBackupMonitorEntity {
    @SerializedName("id")
    @PrimaryKey(autoGenerate = true)
    public long id;

    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("repo_name")
    public String repo_name;
    @SerializedName("related_account")
    public String related_account;
    @SerializedName("parent_dir")
    public String parent_dir;
    @SerializedName("local_path")
    public String local_path;
    @SerializedName("version")
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
