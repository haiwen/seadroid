package com.seafile.seadroid2.framework.model.docs_comment;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter;
import com.seafile.seadroid2.framework.model.sdoc.SDocCommentReplyModel;
import com.seafile.seadroid2.view.rich_edittext.RichEditText;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class DocsCommentModel {
    @SerializedName("id")
    public int id;
    @SerializedName("item_name")
    public String item_name;
    @SerializedName("parent_path")
    public String parent_path;

    @SerializedName("avatar_url")
    public String avatar_url;
    @SerializedName("replies")
    public List<SDocCommentReplyModel> replies;
    @SerializedName("comment")
    public String comment;
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("resolved")
    public boolean resolved;
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

    @SerializedName("detail")
    public String detail;


    @SerializedName("isContainImage")
    public boolean isContainImage = false;
    @SerializedName("commentList")
    public List<RichEditText.RichContentModel> commentList;

    public String getCreatedAtFriendlyText() {
        return created_at.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace("T", " ");
    }
}
