package com.seafile.seadroid2.framework.model.wiki;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.model.BaseModel;

public class WikiInfoModel extends BaseModel {
    public String id;
    public boolean is_published;//wiki1
    public long group_id;
    public String group_name;
    public String group_owner;
    public String color;
    public String icon;

    public String name;
    public String owner;
    public String owner_nickname;
    public String owner_avatar_url;//wiki1
    public String permission;//wiki1、wiki2
    public String public_url;//wiki1、wiki2
    public String public_url_suffix;
    public String slug;
    public String repo_id;
    public String type;
    public String updated_at;
    public String created_at;//wiki1

    public String getIcon() {
        if (TextUtils.isEmpty(icon)) {
            return "haiwen-book-bookmark-fill";
        }
        return "haiwen-" + icon;
    }

    public String getOriginalColor() {
        // default color is #ff9800
        if (TextUtils.isEmpty(color)) {
            return "#ff9800";
        }
        return color;
    }

    public String getColor() {
        // default color is #22ff9800
        if (TextUtils.isEmpty(color)) {
            return "#22ff9800";
        }
        color = color.replace("#", "");
        if (color.length() == 6) {
            color = "22" + color;
        }
        color = "#" + color;
        return color;
    }

}
