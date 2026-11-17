package com.seafile.seadroid2.framework.model.sdoc;

import com.seafile.seadroid2.framework.model.BaseModel;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class OutlineItemModel extends BaseModel {
    @SerializedName("id")
    public String id;
    @SerializedName("type")
    public String type;
    @SerializedName("text")
    public String text;
    @SerializedName("indent")
    public boolean indent;
    @SerializedName("children")
    public List<OutlineItemModel> children;
    @SerializedName("data")
    public SDocDataModel data;
}
