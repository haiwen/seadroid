package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter;

import java.util.List;

public class FileTagResultModel implements Parcelable {
    public String _creator;

    public String _id;
    public String _last_modifier;

    public String _tag_color;
    public String _tag_name;
    public List<FileLinkModel> _tag_file_links;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this._creator);
        dest.writeString(this._id);
        dest.writeString(this._last_modifier);
        dest.writeString(this._tag_color);
        dest.writeString(this._tag_name);
        dest.writeTypedList(this._tag_file_links);
    }

    public FileTagResultModel() {
    }

    protected FileTagResultModel(Parcel in) {
        this._creator = in.readString();
        this._id = in.readString();
        this._last_modifier = in.readString();
        this._tag_color = in.readString();
        this._tag_name = in.readString();
        this._tag_file_links = in.createTypedArrayList(FileLinkModel.CREATOR);
    }

    public static final Parcelable.Creator<FileTagResultModel> CREATOR = new Parcelable.Creator<FileTagResultModel>() {
        @Override
        public FileTagResultModel createFromParcel(Parcel source) {
            return new FileTagResultModel(source);
        }

        @Override
        public FileTagResultModel[] newArray(int size) {
            return new FileTagResultModel[size];
        }
    };
}
