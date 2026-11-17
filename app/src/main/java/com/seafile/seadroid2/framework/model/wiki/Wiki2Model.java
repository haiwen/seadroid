package com.seafile.seadroid2.framework.model.wiki;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class Wiki2Model {
    @SerializedName("wikis")
    public List<WikiInfoModel> wikis;
    @SerializedName("group_wikis")
    public List<GroupWikiModel> group_wikis;
}
