package com.seafile.seadroid2.framework.data.model.sdoc;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.framework.data.model.adapter.OffsetDateTimeAdapter;

import java.time.OffsetDateTime;

public class SDocCommentReplyModel {
    public int id;
    public int comment_id;

    public String doc_uuid;
    public String reply;//"True"

    public String author;
    public String avatar_url;
    public String type;//"type"

    public String resolved;
    public String user_contact_email;
    public String user_email;
    public String user_name;


    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime created_at;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime updated_at;

}
