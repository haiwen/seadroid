package com.seafile.seadroid2.framework.model.wiki;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class GroupWikiModel {
    @SerializedName("group_id")
    public int group_id;
    @SerializedName("group_name")
    public String group_name;
    @SerializedName("owner")
    public String owner;
    @SerializedName("group_admins")
    public List<String> group_admins;
    @SerializedName("wiki_info")
    public List<WikiInfoModel> wiki_info;
}
