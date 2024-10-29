package com.seafile.seadroid2.framework.data.model.user;

import android.os.Parcel;
import android.os.Parcelable;

public class UserModel implements Parcelable {
    private String email;
    private String name;
    private String contact_email;
    private String avatar_url;

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getContactEmail() {
        return contact_email;
    }

    public void setContactEmail(String contact_email) {
        this.contact_email = contact_email;
    }

    public String getAvatarUrl() {
        return avatar_url;
    }

    public void setAvatarUrl(String avatar_url) {
        this.avatar_url = avatar_url;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.email);
        dest.writeString(this.name);
        dest.writeString(this.contact_email);
        dest.writeString(this.avatar_url);
    }

    public UserModel() {
    }

    protected UserModel(Parcel in) {
        this.email = in.readString();
        this.name = in.readString();
        this.contact_email = in.readString();
        this.avatar_url = in.readString();
    }

    public static final Parcelable.Creator<UserModel> CREATOR = new Parcelable.Creator<UserModel>() {
        @Override
        public UserModel createFromParcel(Parcel source) {
            return new UserModel(source);
        }

        @Override
        public UserModel[] newArray(int size) {
            return new UserModel[size];
        }
    };
}
