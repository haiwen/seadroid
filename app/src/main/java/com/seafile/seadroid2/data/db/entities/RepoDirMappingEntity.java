package com.seafile.seadroid2.data.db.entities;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

/**
 * Get the top dir of a repo. If there are multiple repos with same name,
 * say "ABC", their top dir would be "ABC", "ABC (1)", "ABC (2)", etc. The
 * mapping (repoID, dir) is stored in a database table.
 */
@Entity(tableName = "repo_dir_mapping")
public class RepoDirMappingEntity {
    @PrimaryKey(autoGenerate = true)
    public long id;

    public String repo_id;
    public String repo_dir;
    public String related_account;

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
