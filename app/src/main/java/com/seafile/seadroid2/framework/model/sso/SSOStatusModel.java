package com.seafile.seadroid2.framework.model.sso;

import com.google.gson.annotations.SerializedName;

public class SSOStatusModel {
    @SerializedName("status")
    public String status;
    @SerializedName("username")
    public String username;
    @SerializedName("apiToken")
    public String apiToken;
}
