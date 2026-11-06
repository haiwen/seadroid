package com.seafile.seadroid2.framework.model.sdoc;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class SDocOutlineWrapperModel {
    @SerializedName("version")
    public int version;
    @SerializedName("format_version")
    public int format_version;
    @SerializedName("last_modify_user")
    public String last_modify_user;
    @SerializedName("elements")
    public List<OutlineItemModel> elements;
}
