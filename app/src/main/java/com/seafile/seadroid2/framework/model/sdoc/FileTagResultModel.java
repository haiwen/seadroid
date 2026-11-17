package com.seafile.seadroid2.framework.model.sdoc;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class FileTagResultModel {
    @SerializedName("_creator")
    public String _creator;

    @SerializedName("_id")
    public String _id;
    @SerializedName("_last_modifier")
    public String _last_modifier;

    @SerializedName("_tag_color")
    public String _tag_color;
    @SerializedName("_tag_name")
    public String _tag_name;
    @SerializedName("_tag_file_links")
    public List<FileLinkModel> _tag_file_links;
}
