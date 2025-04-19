package com.seafile.seadroid2.framework.data.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.framework.data.model.adapter.OffsetDateTimeAdapter;
import com.seafile.seadroid2.framework.data.model.adapter.RecordResultDeserializer;

import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class RecordResultModel implements Parcelable {

    public List<String> _collaborators;
    public String _creator;
    public String _ctime;
    public String _file_creator;
    public String _file_ctime;
    public String _file_modifier;

    @JsonAdapter(OffsetDateTimeAdapter.class)
    public OffsetDateTime _file_mtime;

    public String _file_type;
    public String _id;
    public boolean _is_dir;
    public String _last_modifier;
    public String _mtime;
    public String _name;
    public String _obj_id;
    public String _parent_dir;
    public long _size;
    public String _status;
    public String _suffix;
    public String _description;
    public List<String> _owner;
    public List<String> _reviewer;

    public String _tag_color;
    public String _tag_name;

    public Object _location;
    public Object _tags;

//    @JsonAdapter(RecordResultDeserializer.class)
//    public Map<String, Object> dynamicFields;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeStringList(this._collaborators);
        dest.writeStringList(this._reviewer);
        dest.writeString(this._creator);
        dest.writeString(this._ctime);
        dest.writeString(this._file_creator);
        dest.writeString(this._file_ctime);
        dest.writeString(this._file_modifier);
        dest.writeSerializable(this._file_mtime);
        dest.writeString(this._file_type);
        dest.writeString(this._id);
        dest.writeByte(this._is_dir ? (byte) 1 : (byte) 0);
        dest.writeString(this._last_modifier);
        dest.writeString(this._mtime);
        dest.writeString(this._name);
        dest.writeString(this._obj_id);
        dest.writeString(this._parent_dir);
        dest.writeLong(this._size);
        dest.writeString(this._status);
        dest.writeString(this._suffix);
        dest.writeString(this._description);
        dest.writeString(this._tag_color);
        dest.writeString(this._tag_name);
        dest.writeStringList(this._owner);
        dest.writeValue(this._location);
        dest.writeValue(this._tags);
//        dest.writeInt(this.dynamicFields.size());
//        for (Map.Entry<String, Object> entry : this.dynamicFields.entrySet()) {
//            dest.writeString(entry.getKey());
//            dest.writeValue(entry.getValue());
//        }
    }

    public RecordResultModel() {
    }

    protected RecordResultModel(Parcel in) {
        this._collaborators = in.createStringArrayList();
        this._reviewer = in.createStringArrayList();
        this._creator = in.readString();
        this._ctime = in.readString();
        this._file_creator = in.readString();
        this._file_ctime = in.readString();
        this._file_modifier = in.readString();
        this._file_mtime = (OffsetDateTime) in.readSerializable();
        this._file_type = in.readString();
        this._id = in.readString();
        this._is_dir = in.readByte() != 0;
        this._last_modifier = in.readString();
        this._mtime = in.readString();
        this._name = in.readString();
        this._obj_id = in.readString();
        this._parent_dir = in.readString();
        this._size = in.readLong();
        this._status = in.readString();
        this._suffix = in.readString();
        this._description = in.readString();
        this._tag_color = in.readString();
        this._tag_name = in.readString();
        this._owner = in.createStringArrayList();
        this._location = in.readValue(Object.class.getClassLoader());
        this._tags = in.readValue(Object.class.getClassLoader());
        int dynamicFieldsSize = in.readInt();
//        this.dynamicFields = new HashMap<String, Object>(dynamicFieldsSize);
//        for (int i = 0; i < dynamicFieldsSize; i++) {
//            String key = in.readString();
//            Object value = in.readParcelable(Object.class.getClassLoader());
//            this.dynamicFields.put(key, value);
//        }
    }

    public static final Parcelable.Creator<RecordResultModel> CREATOR = new Parcelable.Creator<RecordResultModel>() {
        @Override
        public RecordResultModel createFromParcel(Parcel source) {
            return new RecordResultModel(source);
        }

        @Override
        public RecordResultModel[] newArray(int size) {
            return new RecordResultModel[size];
        }
    };
}
