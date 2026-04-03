package com.seafile.seadroid2.framework.model.sdoc;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter;

import java.time.OffsetDateTime;

public class FileDetailModel {
    public String type;
    public String id;
    public String name;
    public String permission;
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
    public long size;
    public boolean starred;
    public long commentTotal;
    public boolean canEdit;


}
