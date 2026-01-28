package com.seafile.seadroid2.framework.model.wiki;

import com.seafile.seadroid2.framework.model.BaseModel;

public class WikiInfoModel extends BaseModel {
    public String id;
    public boolean is_published;//wiki1
    public long group_id;
    public String group_name;
    public String group_owner;

    public String name;
    public String owner;
    public String owner_nickname;
    public String owner_avatar_url;//wiki1
    public String permission;//wiki1、wiki2
    public String public_url;//wiki1、wiki2
    public String public_url_suffix;
    public String repo_id;
    public String type;
    public String updated_at;
    public String created_at;//wiki1

}
