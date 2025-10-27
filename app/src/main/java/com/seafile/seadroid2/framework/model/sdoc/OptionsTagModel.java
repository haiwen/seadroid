package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import androidx.room.Ignore;

public class OptionsTagModel implements Parcelable {
    public String borderColor;

    //default #EED5FF
    public String color = "#EED5FF";
    public String id;
    public String name;
    public String textColor = "#202428";

    @Ignore
    public boolean isSelected;


    public String getColor() {
        //grey
        if (TextUtils.equals("_outdated", id) && TextUtils.equals("_outdated", name)) {
            return "#C2C2C2";
        }
        //red
        if (TextUtils.equals("_in_progress", id) && TextUtils.equals("_in_progress", name)) {
            return "#EED5FF";
        }
        //yellow
        if (TextUtils.equals("_in_review", id) && TextUtils.equals("_in_review", name)) {
            return "#FFFDCD";
        }
        //green
        if (TextUtils.equals("_done", id) && TextUtils.equals("_done", name)) {
            return "#59CB74";
        }
        return color;
    }

    //Reverse color
    public String getTextColor() {
        //grey
        if (TextUtils.equals("_outdated", id) && TextUtils.equals("_outdated", name)) {
            return textColor;
        }
        //red
        if (TextUtils.equals("_in_progress", id) && TextUtils.equals("_in_progress", name)) {
            return "#FFFFFF";
        }
        //yellow
        if (TextUtils.equals("_in_review", id) && TextUtils.equals("_in_review", name)) {
            return textColor;
        }
        //green
        if (TextUtils.equals("_done", id) && TextUtils.equals("_done", name)) {
            return "#FFFFFF";
        }
        return textColor;
    }

    @Override
    public String toString() {
        return "ColumnDataOptionsModel{" +
                "borderColor='" + borderColor + '\'' +
                ", color='" + color + '\'' +
                ", id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", textColor='" + textColor + '\'' +
                ", isSelected=" + isSelected +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.borderColor);
        dest.writeString(this.color);
        dest.writeString(this.id);
        dest.writeString(this.name);
        dest.writeString(this.textColor);
        dest.writeByte(this.isSelected ? (byte) 1 : (byte) 0);
    }

    public void readFromParcel(Parcel source) {
        this.borderColor = source.readString();
        this.color = source.readString();
        this.id = source.readString();
        this.name = source.readString();
        this.textColor = source.readString();
        this.isSelected = source.readByte() != 0;
    }

    public OptionsTagModel() {
    }

    protected OptionsTagModel(Parcel in) {
        this.borderColor = in.readString();
        this.color = in.readString();
        this.id = in.readString();
        this.name = in.readString();
        this.textColor = in.readString();
        this.isSelected = in.readByte() != 0;
    }

    public static final Creator<OptionsTagModel> CREATOR = new Creator<OptionsTagModel>() {
        @Override
        public OptionsTagModel createFromParcel(Parcel source) {
            return new OptionsTagModel(source);
        }

        @Override
        public OptionsTagModel[] newArray(int size) {
            return new OptionsTagModel[size];
        }
    };
}
