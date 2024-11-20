package com.seafile.seadroid2.framework.data.db.entities;

import androidx.annotation.NonNull;
import androidx.room.Entity;

import com.seafile.seadroid2.framework.data.model.BaseModel;

@Entity(tableName = "permissions", primaryKeys = {"repo_id", "id"})
public class PermissionEntity extends BaseModel {

    @NonNull
    public String repo_id = "";

    public int id;
    public String description;
    public String name;

    //permissions
    public boolean create;
    public boolean upload;
    public boolean download;
    public boolean preview;
    public boolean copy;
    public boolean delete;
    public boolean modify;
    public boolean download_external_link;
    //1001010
}
