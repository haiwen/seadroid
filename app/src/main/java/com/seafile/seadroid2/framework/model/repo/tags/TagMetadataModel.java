package com.seafile.seadroid2.framework.model.repo.tags;

import android.os.Parcel;
import android.os.Parcelable;

public class TagMetadataModel implements Parcelable {
    public String key;
    public String name;
    public String type;
    public TagMetadataDataModel data;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.key);
        dest.writeString(this.name);
        dest.writeString(this.type);
        dest.writeParcelable(this.data, flags);
    }

    public TagMetadataModel() {
    }

    protected TagMetadataModel(Parcel in) {
        this.key = in.readString();
        this.name = in.readString();
        this.type = in.readString();
        this.data = in.readParcelable(TagMetadataDataModel.class.getClassLoader());
    }

    public static final Parcelable.Creator<TagMetadataModel> CREATOR = new Parcelable.Creator<TagMetadataModel>() {
        @Override
        public TagMetadataModel createFromParcel(Parcel source) {
            return new TagMetadataModel(source);
        }

        @Override
        public TagMetadataModel[] newArray(int size) {
            return new TagMetadataModel[size];
        }
    };
}
