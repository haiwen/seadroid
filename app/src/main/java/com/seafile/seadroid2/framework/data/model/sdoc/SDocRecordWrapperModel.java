package com.seafile.seadroid2.framework.data.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.List;

public class SDocRecordWrapperModel implements Parcelable {
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

    public SDocRecordWrapperModel() {
    }

    protected SDocRecordWrapperModel(Parcel in) {
        this.metadata = in.createTypedArrayList(MetadataModel.CREATOR);
        this.results = in.createTypedArrayList(RecordResultModel.CREATOR);
    }

    public static final Parcelable.Creator<SDocRecordWrapperModel> CREATOR = new Parcelable.Creator<SDocRecordWrapperModel>() {
        @Override
        public SDocRecordWrapperModel createFromParcel(Parcel source) {
            return new SDocRecordWrapperModel(source);
        }

        @Override
        public SDocRecordWrapperModel[] newArray(int size) {
            return new SDocRecordWrapperModel[size];
        }
    };
}
