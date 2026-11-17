package com.seafile.seadroid2.framework.model.sdoc;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter;

import java.time.OffsetDateTime;

public class SDocCommentReplyModel {
    @SerializedName("id")
    public int id;
    @SerializedName("comment_id")
    public int comment_id;

    @SerializedName("doc_uuid")
    public String doc_uuid;
    @SerializedName("reply")
    public String reply;//"True"

    @SerializedName("author")
    public String author;
    @SerializedName("avatar_url")
    public String avatar_url;
    @SerializedName("type")
    public String type;//"type"

    @SerializedName("resolved")
    public String resolved;
    @SerializedName("user_contact_email")
    public String user_contact_email;
    @SerializedName("user_email")
    public String user_email;
    @SerializedName("user_name")
    public String user_name;


    @SerializedName("created_at")
    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime created_at;

    @SerializedName("updated_at")
    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime updated_at;

}
