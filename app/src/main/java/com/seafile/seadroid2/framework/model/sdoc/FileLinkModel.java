package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

public class FileLinkModel implements Parcelable {
    public String display_value;
    public String row_id;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.display_value);
        dest.writeString(this.row_id);
    }

    public FileLinkModel() {
    }

    protected FileLinkModel(Parcel in) {
        this.display_value = in.readString();
        this.row_id = in.readString();
    }

    public static final Parcelable.Creator<FileLinkModel> CREATOR = new Creator<FileLinkModel>() {
        @Override
        public FileLinkModel createFromParcel(Parcel source) {
            return new FileLinkModel(source);
        }

        @Override
        public FileLinkModel[] newArray(int size) {
            return new FileLinkModel[size];
        }
    };
}
