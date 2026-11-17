package com.seafile.seadroid2.framework.model.permission;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class PermissionListWrapperModel {
    @SerializedName("permission_list")
    public List<PermissionParentModel> permission_list;
}
