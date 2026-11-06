package com.seafile.seadroid2.framework.model.transfer;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class UploadBlksLinkModel {
    @SerializedName("rawblksurl")
    public String rawblksurl;
    @SerializedName("commiturl")
    public String commiturl;
    @SerializedName("blklist")
    public List<String> blklist;
}
