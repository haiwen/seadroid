package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter;
import com.seafile.seadroid2.framework.util.Utils;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;

public class FileDetailModel implements Parcelable {

    private String type;
    private String id;
    private String name;
    private String permission;
    private long mtime;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    @SerializedName("last_modified")
    private OffsetDateTime lastModified;

    @SerializedName("last_modifier_email")
    private String lastModifierEmail;
    @SerializedName("last_modifier_name")
    private String lastModifierName;
    @SerializedName("last_modifier_contact_email")
    private String lastModifierContactEmail;
    @SerializedName("last_modifier_avatar")
    private String lastModifierAvatar;
    private long size;
    private boolean starred;
    private long commentTotal;
    private boolean canEdit;


    public String getPermission() {
        return permission;
    }

    public void setPermission(String value) {
        this.permission = value;
    }

    public String getType() {
        return type;
    }

    public void setType(String value) {
        this.type = value;
    }

    public long getMtime() {
        return mtime;
    }

    public void setMtime(long value) {
        this.mtime = value;
    }


    public long getSize() {
        return size;
    }

    public String getSizeText() {
        return Utils.readableFileSize(size);
    }

    public void setSize(long value) {
        this.size = value;
    }

    public boolean getStarred() {
        return starred;
    }

    public void setStarred(boolean value) {
        this.starred = value;
    }

    public String getName() {
        return name;
    }

    public void setName(String value) {
        this.name = value;
    }

    public long getCommentTotal() {
        return commentTotal;
    }

    public void setCommentTotal(long value) {
        this.commentTotal = value;
    }

    public String getId() {
        return id;
    }

    public void setId(String value) {
        this.id = value;
    }

    public OffsetDateTime getLastModified() {
        return lastModified;
    }

    public String getLastModifiedText(){
        return getLastModified().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace("T"," ");
    }

    public void setLastModified(OffsetDateTime value) {
        this.lastModified = value;
    }

    public String getLastModifierEmail() {
        return lastModifierEmail;
    }

    public void setLastModifierEmail(String value) {
        this.lastModifierEmail = value;
    }

    public String getLastModifierName() {
        return lastModifierName;
    }

    public void setLastModifierName(String value) {
        this.lastModifierName = value;
    }

    public String getLastModifierContactEmail() {
        return lastModifierContactEmail;
    }

    public void setLastModifierContactEmail(String value) {
        this.lastModifierContactEmail = value;
    }

    public String getLastModifierAvatar() {
        return lastModifierAvatar;
    }

    public void setLastModifierAvatar(String value) {
        this.lastModifierAvatar = value;
    }

    public boolean getCanEdit() {
        return canEdit;
    }

    public void setCanEdit(boolean value) {
        this.canEdit = value;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.type);
        dest.writeString(this.id);
        dest.writeString(this.name);
        dest.writeString(this.permission);
        dest.writeLong(this.mtime);
        dest.writeSerializable(this.lastModified);
        dest.writeString(this.lastModifierEmail);
        dest.writeString(this.lastModifierName);
        dest.writeString(this.lastModifierContactEmail);
        dest.writeString(this.lastModifierAvatar);
        dest.writeLong(this.size);
        dest.writeByte(this.starred ? (byte) 1 : (byte) 0);
        dest.writeLong(this.commentTotal);
        dest.writeByte(this.canEdit ? (byte) 1 : (byte) 0);
    }

    public FileDetailModel() {
    }

    protected FileDetailModel(Parcel in) {
        this.type = in.readString();
        this.id = in.readString();
        this.name = in.readString();
        this.permission = in.readString();
        this.mtime = in.readLong();
        this.lastModified = (OffsetDateTime) in.readSerializable();
        this.lastModifierEmail = in.readString();
        this.lastModifierName = in.readString();
        this.lastModifierContactEmail = in.readString();
        this.lastModifierAvatar = in.readString();
        this.size = in.readLong();
        this.starred = in.readByte() != 0;
        this.commentTotal = in.readLong();
        this.canEdit = in.readByte() != 0;
    }

    public static final Parcelable.Creator<FileDetailModel> CREATOR = new Parcelable.Creator<FileDetailModel>() {
        @Override
        public FileDetailModel createFromParcel(Parcel source) {
            return new FileDetailModel(source);
        }

        @Override
        public FileDetailModel[] newArray(int size) {
            return new FileDetailModel[size];
        }
    };
}
