package com.seafile.seadroid2.framework.model.repo.tags;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.framework.model.adapter.OffsetDateTimeAdapter;

import java.time.OffsetDateTime;
import java.util.List;

public class TagResultModel implements Parcelable {
    public String _creator;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime _ctime;

    public String _id;
    public String _last_modifier;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime _mtime;

    public String _tag_color;
    public String _tag_name;
    public List<TagFileLinkModel> _tag_file_links;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this._creator);
        dest.writeSerializable(this._ctime);
        dest.writeString(this._id);
        dest.writeString(this._last_modifier);
        dest.writeSerializable(this._mtime);
        dest.writeString(this._tag_color);
        dest.writeString(this._tag_name);
        dest.writeTypedList(this._tag_file_links);
    }

    public TagResultModel() {
    }

    protected TagResultModel(Parcel in) {
        this._creator = in.readString();
        this._ctime = (OffsetDateTime) in.readSerializable();
        this._id = in.readString();
        this._last_modifier = in.readString();
        this._mtime = (OffsetDateTime) in.readSerializable();
        this._tag_color = in.readString();
        this._tag_name = in.readString();
        this._tag_file_links = in.createTypedArrayList(TagFileLinkModel.CREATOR);
    }

    public static final Parcelable.Creator<TagResultModel> CREATOR = new Parcelable.Creator<TagResultModel>() {
        @Override
        public TagResultModel createFromParcel(Parcel source) {
            return new TagResultModel(source);
        }

        @Override
        public TagResultModel[] newArray(int size) {
            return new TagResultModel[size];
        }
    };
}
