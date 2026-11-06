package com.seafile.seadroid2.framework.model;

import com.google.gson.annotations.SerializedName;

public class TResultModel<T> extends ResultModel {
    @SerializedName("data")
    public T data;
}
