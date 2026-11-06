package com.seafile.seadroid2.framework.model.search;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class SearchWrapperModel {
    @SerializedName("has_more")
    public boolean has_more;
    @SerializedName("results")
    public List<SearchModel> results;
    @SerializedName("total")
    public int total;
}
