package com.seafile.seadroid2.framework.model.search;

import com.seafile.seadroid2.framework.model.BaseModel;
import com.google.gson.annotations.SerializedName;

public class SearchFileModel extends BaseModel {
    @SerializedName("path")
    public String path;
    @SerializedName("type")
    public String type;
    @SerializedName("size")
    public long size;    // size of file, 0 if type is dir
    @SerializedName("mtime")
    public String mtime;
}
