package com.seafile.seadroid2.framework.model.sdoc;

import java.util.List;
import java.util.Map;
import com.google.gson.annotations.SerializedName;

public class RecordResultWrapperModel {
    @SerializedName("results")
    public List<Map<String, Object>> results;
}
