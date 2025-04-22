package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.List;

public class FileTagWrapperModel implements Parcelable {
    public List<MetadataModel> metadata;
    public List<RecordResultModel> results;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeTypedList(this.metadata);
        dest.writeTypedList(this.results);
    }

    public FileTagWrapperModel() {
    }

    protected FileTagWrapperModel(Parcel in) {
        this.metadata = in.createTypedArrayList(MetadataModel.CREATOR);
        this.results = in.createTypedArrayList(RecordResultModel.CREATOR);
    }

    public static final Creator<FileTagWrapperModel> CREATOR = new Creator<FileTagWrapperModel>() {
        @Override
        public FileTagWrapperModel createFromParcel(Parcel source) {
            return new FileTagWrapperModel(source);
        }

        @Override
        public FileTagWrapperModel[] newArray(int size) {
            return new FileTagWrapperModel[size];
        }
    };
}
