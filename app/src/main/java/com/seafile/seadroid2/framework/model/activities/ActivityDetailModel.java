package com.seafile.seadroid2.framework.model.activities;

import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;
import com.google.gson.annotations.SerializedName;

import org.apache.commons.lang3.StringUtils;

public class ActivityDetailModel {
    @SerializedName("obj_id")
    public String obj_id;
    @SerializedName("path")
    public String path;
    @SerializedName("repo_name")
    public String repo_name;
    @SerializedName("old_path")
    public String old_path;
    @SerializedName("size")
    public long size;
    @SerializedName("time")
    public String time;

    @SerializedName("formatedTime")
    private String formatedTime;

    public String getTime() {
        if (StringUtils.isNoneEmpty(formatedTime)) {
            return formatedTime;
        }

        if (StringUtils.isEmpty(time)) {
            return "";
        }

        long mTempTimeLong = Times.convertMtime2Long(time);
        formatedTime = Utils.translateCommitTime(mTempTimeLong);
        return formatedTime;
    }

}
