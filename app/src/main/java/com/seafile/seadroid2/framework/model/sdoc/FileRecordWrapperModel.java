package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.List;

public class FileRecordWrapperModel implements Parcelable {
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

    public FileRecordWrapperModel() {
    }

    protected FileRecordWrapperModel(Parcel in) {
        this.metadata = in.createTypedArrayList(MetadataModel.CREATOR);
        this.results = in.createTypedArrayList(RecordResultModel.CREATOR);
    }

    public static final Parcelable.Creator<FileRecordWrapperModel> CREATOR = new Parcelable.Creator<FileRecordWrapperModel>() {
        @Override
        public FileRecordWrapperModel createFromParcel(Parcel source) {
            return new FileRecordWrapperModel(source);
        }

        @Override
        public FileRecordWrapperModel[] newArray(int size) {
            return new FileRecordWrapperModel[size];
        }
    };
}
