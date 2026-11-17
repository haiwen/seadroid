package com.seafile.seadroid2.framework.model.wiki;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.model.BaseModel;
import com.google.gson.annotations.SerializedName;

public class WikiInfoModel extends BaseModel {
    @SerializedName("id")
    public String id;
    @SerializedName("is_published")
    public boolean is_published;//wiki1
    @SerializedName("group_id")
    public long group_id;
    @SerializedName("group_name")
    public String group_name;
    @SerializedName("group_owner")
    public String group_owner;
    @SerializedName("color")
    public String color;
    @SerializedName("icon")
    public String icon;

    @SerializedName("name")
    public String name;
    @SerializedName("owner")
    public String owner;
    @SerializedName("owner_nickname")
    public String owner_nickname;
    @SerializedName("owner_avatar_url")
    public String owner_avatar_url;//wiki1
    @SerializedName("permission")
    public String permission;//wiki1、wiki2
    @SerializedName("public_url")
    public String public_url;//wiki1、wiki2
    @SerializedName("public_url_suffix")
    public String public_url_suffix;
    @SerializedName("slug")
    public String slug;
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("type")
    public String type;
    @SerializedName("updated_at")
    public String updated_at;
    @SerializedName("created_at")
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
