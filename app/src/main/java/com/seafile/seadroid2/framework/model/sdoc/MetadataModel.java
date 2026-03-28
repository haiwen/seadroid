package com.seafile.seadroid2.framework.model.sdoc;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.framework.model.adapter.MetadataConfigDataJsonAdapter;

import java.util.List;

public class MetadataModel {
    public String key;
    public String name;
    public String type;

    @SerializedName("data")
    @JsonAdapter(MetadataConfigDataJsonAdapter.class)
    public List<MetadataConfigDataModel> config;

    //notice this field
    public Object value;

    public MetadataConfigDataModel getConfigData() {
        if (CollectionUtils.isEmpty(config)){
            return null;
        }
        return config.get(0);
    }

    @NonNull
    @Override
    public String toString() {
        return "MetadataModel{" +
                "key='" + key + '\'' +
                ", name='" + name + '\'' +
                ", type='" + type + '\'' +
                '}';
    }
}
