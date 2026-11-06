package com.seafile.seadroid2.framework.model;

import com.google.gson.annotations.SerializedName;

public class WebRouteModel {
    @SerializedName("v")
    public int v = 1;
    @SerializedName("action")
    public String action;

    /**
     * params data
     */
    @SerializedName("data")
    public String data;
}
