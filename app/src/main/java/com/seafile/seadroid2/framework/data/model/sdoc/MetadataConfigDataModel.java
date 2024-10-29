package com.seafile.seadroid2.framework.data.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.SerializedName;

import java.util.List;

public class MetadataConfigDataModel implements Parcelable {

    public String format;

    public String geo_format;
    public List<OptionsTagModel> options;

    public boolean enable_precision;
    public int precision;
    public String currency_symbol;
    public String currency_symbol_position;

    @SerializedName("max")
    public int rate_max_number;
    @SerializedName("color")
    public String rate_style_color;

//    @SerializedName("type")
//    public String rate_style_type;

    public MetadataConfigDataModel() {
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.format);
        dest.writeString(this.geo_format);
        dest.writeTypedList(this.options);
        dest.writeInt(this.rate_max_number);
        dest.writeString(this.rate_style_color);
    }

    protected MetadataConfigDataModel(Parcel in) {
        this.format = in.readString();
        this.geo_format = in.readString();
        this.options = in.createTypedArrayList(OptionsTagModel.CREATOR);
        this.rate_max_number = in.readInt();
        this.rate_style_color = in.readString();
    }

    public static final Creator<MetadataConfigDataModel> CREATOR = new Creator<MetadataConfigDataModel>() {
        @Override
        public MetadataConfigDataModel createFromParcel(Parcel source) {
            return new MetadataConfigDataModel(source);
        }

        @Override
        public MetadataConfigDataModel[] newArray(int size) {
            return new MetadataConfigDataModel[size];
        }
    };
}
