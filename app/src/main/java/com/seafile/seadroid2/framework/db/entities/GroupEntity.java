package com.seafile.seadroid2.framework.db.entities;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.adapter.GroupQuotaUsageAdapter;

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

    @SerializedName("parent_groud_id")
    public long parent_groud_id;

    @SerializedName("created_at")
    public String created_at;
    @SerializedName("admins")
    public List<String> admins;
    @SerializedName("group_quota")
    public long group_quota = 0; // < 0:no limit
    @SerializedName("group_quota_usage")
    @JsonAdapter(GroupQuotaUsageAdapter.class)
    public long group_quota_usage;
}
