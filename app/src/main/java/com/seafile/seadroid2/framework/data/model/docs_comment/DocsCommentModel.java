package com.seafile.seadroid2.framework.data.model.docs_comment;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.framework.data.model.adapter.OffsetDateTimeAdapter;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocCommentReplyModel;
import com.seafile.seadroid2.view.rich_edittext.RichEditText;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class DocsCommentModel {
    public int id;
    public String item_name;
    public String parent_path;

    public String avatar_url;
    public List<SDocCommentReplyModel> replies;
    public String comment;
    public String repo_id;
    public boolean resolved;
    public String user_contact_email;
    public String user_email;
    public String user_name;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime created_at;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime updated_at;

    public String detail;


    public boolean isContainImage = false;
    public List<RichEditText.RichContentModel> commentList;

    public String getCreatedAtFriendlyText() {
        return created_at.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace("T", " ");
    }
}
