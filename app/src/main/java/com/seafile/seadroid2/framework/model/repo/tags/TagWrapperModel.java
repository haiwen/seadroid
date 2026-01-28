package com.seafile.seadroid2.framework.model.repo.tags;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.List;

public class TagWrapperModel implements Parcelable {
    public List<TagMetadataModel> metadata;
    public List<TagResultModel> results;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeTypedList(this.metadata);
        dest.writeTypedList(this.results);
    }

    public TagWrapperModel() {
    }

    protected TagWrapperModel(Parcel in) {
        this.metadata = in.createTypedArrayList(TagMetadataModel.CREATOR);
        this.results = in.createTypedArrayList(TagResultModel.CREATOR);
    }

    public static final Parcelable.Creator<TagWrapperModel> CREATOR = new Parcelable.Creator<TagWrapperModel>() {
        @Override
        public TagWrapperModel createFromParcel(Parcel source) {
            return new TagWrapperModel(source);
        }

        @Override
        public TagWrapperModel[] newArray(int size) {
            return new TagWrapperModel[size];
        }
    };
}
