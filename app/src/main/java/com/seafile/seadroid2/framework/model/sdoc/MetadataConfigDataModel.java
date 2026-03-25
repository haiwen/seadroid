package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.SerializedName;

import java.util.List;

public class MetadataConfigDataModel implements Parcelable {

    public String format;

    public String geo_format;
    public List<OptionTagModel> options;

    public boolean enable_precision;
    public int precision;
    public String currency_symbol;
    public String currency_symbol_position;

    @SerializedName("max")
    public int rate_max_number;
    @SerializedName("color")
    public String rate_style_color;

    //
    public String display_column_key;
    public String link_id;
    public String other_table_id;
    public String table_id;

//    @SerializedName("type")
//    public String rate_style_type;

    public MetadataConfigDataModel() {
    }

    public String getFormat() {

        return format
                .replace("M/D/YYYY","M/d/yyyy")
                .replace("YYYY", "yyyy")
                .replace("DD", "dd");
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
        dest.writeString(this.display_column_key);
        dest.writeString(this.link_id);
        dest.writeString(this.other_table_id);
        dest.writeString(this.table_id);
    }

    protected MetadataConfigDataModel(Parcel in) {
        this.format = in.readString();
        this.geo_format = in.readString();
        this.options = in.createTypedArrayList(OptionTagModel.CREATOR);
        this.rate_max_number = in.readInt();
        this.rate_style_color = in.readString();
        this.display_column_key = in.readString();
        this.link_id = in.readString();
        this.other_table_id = in.readString();
        this.table_id = in.readString();
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
