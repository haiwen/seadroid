package com.seafile.seadroid2.framework.model.sdoc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.google.gson.annotations.SerializedName;

public class FileRecordWrapperModel {
    @SerializedName("metadata")
    public List<MetadataModel> metadata = new ArrayList<>();
    @SerializedName("results")
    public List<Map<String, Object>> results = new ArrayList<>();
}
