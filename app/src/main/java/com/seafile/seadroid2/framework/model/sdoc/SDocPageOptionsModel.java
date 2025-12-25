package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

public class SDocPageOptionsModel implements Parcelable {
    public String docName;
    public String docUuid;
    public String seadocServerUrl;
    public String seadocAccessToken;
    public String repoID;
    public String repoName;
    public boolean isLocked;
    public boolean isStarred;

    public boolean enableMetadataManagement;
    public String latestContributor;
    public String latestContributorName;
    public String lastModificationTime;

    public boolean canUse() {
        if (TextUtils.isEmpty(seadocServerUrl)
                || TextUtils.isEmpty(docUuid)
                || TextUtils.isEmpty(repoID)
                || TextUtils.isEmpty(seadocAccessToken)) {
            return false;
        }

        return true;
    }

    @Override
    public String toString() {
        return "SDocPageOptionsModel{" +
                "docName='" + docName + '\'' +
                ", docUuid='" + docUuid + '\'' +
                ", seadocServerUrl='" + seadocServerUrl + '\'' +
                ", seadocAccessToken='" + seadocAccessToken + '\'' +
                ", repoID='" + repoID + '\'' +
                ", repoName='" + repoName + '\'' +
                ", isLocked=" + isLocked +
                ", isStarred=" + isStarred +
                ", enableMetadataManagement=" + enableMetadataManagement +
                ", latestContributor='" + latestContributor + '\'' +
                ", latestContributorName='" + latestContributorName + '\'' +
                ", lastModificationTime='" + lastModificationTime + '\'' +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.docName);
        dest.writeString(this.docUuid);
        dest.writeString(this.seadocServerUrl);
        dest.writeString(this.seadocAccessToken);
        dest.writeString(this.repoID);
        dest.writeString(this.repoName);
        dest.writeByte(this.isLocked ? (byte) 1 : (byte) 0);
        dest.writeByte(this.isStarred ? (byte) 1 : (byte) 0);
        dest.writeByte(this.enableMetadataManagement ? (byte) 1 : (byte) 0);
        dest.writeString(this.latestContributor);
        dest.writeString(this.latestContributorName);
        dest.writeString(this.lastModificationTime);
    }

    public SDocPageOptionsModel() {
    }

    protected SDocPageOptionsModel(Parcel in) {
        this.docName = in.readString();
        this.docUuid = in.readString();
        this.seadocServerUrl = in.readString();
        this.seadocAccessToken = in.readString();
        this.repoID = in.readString();
        this.repoName = in.readString();
        this.isLocked = in.readByte() != 0;
        this.isStarred = in.readByte() != 0;
        this.enableMetadataManagement = in.readByte() != 0;
        this.latestContributor = in.readString();
        this.latestContributorName = in.readString();
        this.lastModificationTime = in.readString();
    }

    public static final Creator<SDocPageOptionsModel> CREATOR = new Creator<SDocPageOptionsModel>() {
        @Override
        public SDocPageOptionsModel createFromParcel(Parcel source) {
            return new SDocPageOptionsModel(source);
        }

        @Override
        public SDocPageOptionsModel[] newArray(int size) {
            return new SDocPageOptionsModel[size];
        }
    };
}