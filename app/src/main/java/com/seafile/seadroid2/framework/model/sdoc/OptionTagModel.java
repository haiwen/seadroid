package com.seafile.seadroid2.framework.model.sdoc;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Ignore;

public class OptionTagModel {
    public String type;// tag/single-select/muilt-select
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
            return "#FFFFFF";
        }
        //red
        if (TextUtils.equals("_in_progress", id) && TextUtils.equals("_in_progress", name)) {
            return "#202428";
        }
        //yellow
        if (TextUtils.equals("_in_review", id) && TextUtils.equals("_in_review", name)) {
            return "#202428";
        }
        //green
        if (TextUtils.equals("_done", id) && TextUtils.equals("_done", name)) {
            return "#FFFFFF";
        }
        return textColor;
    }

    @NonNull
    @Override
    public String toString() {
        return "ColumnDataOptionsModel{" +
                ", id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", isSelected=" + isSelected +
                '}';
    }
}
