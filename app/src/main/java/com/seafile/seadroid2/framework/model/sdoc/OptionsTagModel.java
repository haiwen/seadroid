package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

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
