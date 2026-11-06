package com.seafile.seadroid2.framework.model.search;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class SearchFileWrapperModel {
    @SerializedName("data")
    public List<SearchFileModel> data;
}
