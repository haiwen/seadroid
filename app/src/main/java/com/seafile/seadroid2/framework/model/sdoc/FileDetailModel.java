package com.seafile.seadroid2.framework.model.sdoc;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter;

import java.time.OffsetDateTime;

public class FileDetailModel {
    @SerializedName("type")
    public String type;
    @SerializedName("id")
    public String id;
    @SerializedName("name")
    public String name;
    @SerializedName("permission")
    public String permission;
    @SerializedName("mtime")
    public long mtime;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    @SerializedName("last_modified")
    public OffsetDateTime lastModified;

    @SerializedName("last_modifier_email")
    public String lastModifierEmail;
    @SerializedName("last_modifier_name")
    public String lastModifierName;
    @SerializedName("last_modifier_contact_email")
    public String lastModifierContactEmail;
    @SerializedName("last_modifier_avatar")
    public String lastModifierAvatar;
    @SerializedName("size")
    public long size;
    @SerializedName("starred")
    public boolean starred;
    @SerializedName("commentTotal")
    public long commentTotal;
    @SerializedName("canEdit")
    public boolean canEdit;


}
