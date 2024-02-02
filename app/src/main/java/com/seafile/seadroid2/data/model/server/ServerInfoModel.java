package com.seafile.seadroid2.data.model.server;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.data.ServerInfo;

import java.util.Collection;
import java.util.List;

public class ServerInfoModel {
    public String version;
    public String encrypted_library_version;
    public List<String> features;

    public String getFeaturesString() {
        if (CollectionUtils.isEmpty(features)){
            return null;
        }

        return String.join(",",features);
    }
}
