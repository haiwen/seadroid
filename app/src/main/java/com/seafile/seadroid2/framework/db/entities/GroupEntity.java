package com.seafile.seadroid2.framework.db.entities;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.BaseModel;

import java.util.List;

@Entity(tableName = "groups")
public class GroupEntity extends BaseModel {

    @PrimaryKey(autoGenerate = false)
    @ColumnInfo(name = "group_id")
    @SerializedName("id")
    public long group_id;
    @SerializedName("name")
    @ColumnInfo(name = "group_name")
    public String group_name;

    public long parent_groud_id;

    public String created_at;
    public List<String> admins;
    public long group_quota; // < 0:no limit
    public long group_quota_usage;
}
