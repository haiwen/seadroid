package com.seafile.seadroid2.framework.model;

import com.google.gson.annotations.SerializedName;

public class ResultModel {
    @SerializedName("success")
    public boolean success;
    @SerializedName("error_msg")
    public String error_msg;

    @Override
    public String toString() {
        return "ResultModel{" +
                "success=" + success +
                ", error_msg='" + error_msg + '\'' +
                '}';
    }
}
