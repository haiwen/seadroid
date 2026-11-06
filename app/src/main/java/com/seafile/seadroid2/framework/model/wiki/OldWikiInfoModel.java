package com.seafile.seadroid2.framework.model.wiki;

import com.google.gson.annotations.SerializedName;

public class OldWikiInfoModel {
    @SerializedName("id")
    public int id;
    @SerializedName("link")
    public String link;
    @SerializedName("name")
    public String name;
    @SerializedName("owner")
    public String owner;
    @SerializedName("owner_nickname")
    public String owner_nickname;
    @SerializedName("owner_avatar_url")
    public String owner_avatar_url;
    @SerializedName("permission")
    public String permission;
    @SerializedName("slug")
    public String slug;
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("updated_at")
    public String updated_at;
    @SerializedName("created_at")
    public String created_at;

}
