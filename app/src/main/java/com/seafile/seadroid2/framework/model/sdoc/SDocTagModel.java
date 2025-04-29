package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

public class SDocTagModel implements Parcelable {
    public String name;
    public String id;
    public String color;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.name);
        dest.writeString(this.id);
        dest.writeString(this.color);
    }

    public SDocTagModel() {
    }

    protected SDocTagModel(Parcel in) {
        this.name = in.readString();
        this.id = in.readString();
        this.color = in.readString();
    }

    public static final Parcelable.Creator<SDocTagModel> CREATOR = new Parcelable.Creator<SDocTagModel>() {
        @Override
        public SDocTagModel createFromParcel(Parcel source) {
            return new SDocTagModel(source);
        }

        @Override
        public SDocTagModel[] newArray(int size) {
            return new SDocTagModel[size];
        }
    };
}
