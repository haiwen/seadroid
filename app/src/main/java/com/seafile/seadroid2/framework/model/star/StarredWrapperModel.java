package com.seafile.seadroid2.framework.model.star;

import com.seafile.seadroid2.framework.db.entities.StarredModel;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class StarredWrapperModel {
    @SerializedName("starred_item_list")
    public List<StarredModel> starred_item_list;
}
