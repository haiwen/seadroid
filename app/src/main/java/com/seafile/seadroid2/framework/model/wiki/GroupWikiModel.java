package com.seafile.seadroid2.framework.model.wiki;

import java.util.List;

public class GroupWikiModel {
    public int group_id;
    public String group_name;
    public String owner;
    public List<String> group_admins;
    public List<WikiInfoModel> wiki_info;
}
