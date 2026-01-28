package com.seafile.seadroid2.framework.model.repo.tags;

import android.os.Parcel;
import android.os.Parcelable;

public class TagFileLinkModel implements Parcelable {
    public String row_id;
    public String display_value;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.row_id);
        dest.writeString(this.display_value);
    }

    public TagFileLinkModel() {
    }

    protected TagFileLinkModel(Parcel in) {
        this.row_id = in.readString();
        this.display_value = in.readString();
    }

    public static final Parcelable.Creator<TagFileLinkModel> CREATOR = new Parcelable.Creator<TagFileLinkModel>() {
        @Override
        public TagFileLinkModel createFromParcel(Parcel source) {
            return new TagFileLinkModel(source);
        }

        @Override
        public TagFileLinkModel[] newArray(int size) {
            return new TagFileLinkModel[size];
        }
    };
}
