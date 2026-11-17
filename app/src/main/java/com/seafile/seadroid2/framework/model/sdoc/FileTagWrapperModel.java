package com.seafile.seadroid2.framework.model.sdoc;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class FileTagWrapperModel {
    @SerializedName("metadata")
    public List<MetadataModel> metadata;
    @SerializedName("results")
    public List<FileTagResultModel> results;
}
