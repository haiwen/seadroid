package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.adapter.MetadataConfigDataJsonAdapter;

import java.util.List;

public class MetadataModel implements Parcelable {
    public String key;
    public String name;
    public String type;

    @SerializedName("data")
    @JsonAdapter(MetadataConfigDataJsonAdapter.class)
    public List<MetadataConfigDataModel> config;

    //note this
    public Object value;

    public MetadataConfigDataModel getConfigData() {
        if (CollectionUtils.isEmpty(config)){
            return null;
        }
        return config.get(0);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.key);
        dest.writeString(this.name);
        dest.writeString(this.type);
        dest.writeTypedList(this.config);
        dest.writeValue(this.value);
    }

    public MetadataModel() {
    }

    protected MetadataModel(Parcel in) {
        this.key = in.readString();
        this.name = in.readString();
        this.type = in.readString();
        this.config = in.createTypedArrayList(MetadataConfigDataModel.CREATOR);
        this.value = in.readParcelable(Object.class.getClassLoader());
    }

    public static final Creator<MetadataModel> CREATOR = new Creator<MetadataModel>() {
        @Override
        public MetadataModel createFromParcel(Parcel source) {
            return new MetadataModel(source);
        }

        @Override
        public MetadataModel[] newArray(int size) {
            return new MetadataModel[size];
        }
    };

    @Override
    public String toString() {
        return "MetadataModel{" +
                "key='" + key + '\'' +
                ", name='" + name + '\'' +
                ", type='" + type + '\'' +
                '}';
    }
}
