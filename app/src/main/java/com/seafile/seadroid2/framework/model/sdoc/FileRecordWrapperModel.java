package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class FileRecordWrapperModel implements Parcelable {
    public List<MetadataModel> metadata = new ArrayList<>();
    public List<Map<String, Object>> results = new ArrayList<>();

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeTypedList(this.metadata);
        dest.writeList(this.results);
    }

    public FileRecordWrapperModel() {
    }

    protected FileRecordWrapperModel(Parcel in) {
        this.metadata = in.createTypedArrayList(MetadataModel.CREATOR);
        this.results = new ArrayList<Map<String, Object>>();
        in.readList(this.results, Map.class.getClassLoader());
    }

    public static final Creator<FileRecordWrapperModel> CREATOR = new Creator<FileRecordWrapperModel>() {
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
