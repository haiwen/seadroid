package com.seafile.seadroid2.framework.model.server;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class ServerInfoModel {
    @SerializedName("version")
    public String version;
    @SerializedName("encrypted_library_version")
    public String encrypted_library_version;

    //pbkdf2_sha256
    @SerializedName("encrypted_library_pwd_hash_algo")
    public String encrypted_library_pwd_hash_algo = null;
    //1000
    @SerializedName("encrypted_library_pwd_hash_params")
    public String encrypted_library_pwd_hash_params = null;

    @SerializedName("features")
    public List<String> features;

    public String getFeaturesString() {
        if (CollectionUtils.isEmpty(features)) {
            return null;
        }

        return String.join(",", features);
    }
}
