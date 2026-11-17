package com.seafile.seadroid2.framework.model.permission;

import com.google.gson.annotations.SerializedName;

public class PermissionParentModel {
    @SerializedName("id")
    public int id;
    @SerializedName("description")
    public String description;
    @SerializedName("name")
    public String name;
    @SerializedName("permission")
    public PermissionModel permission;
}
