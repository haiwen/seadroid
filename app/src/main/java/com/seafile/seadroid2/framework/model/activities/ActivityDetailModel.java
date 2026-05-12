package com.seafile.seadroid2.framework.model.activities;

import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.lang3.StringUtils;

public class ActivityDetailModel {
    public String obj_id;
    public String path;
    public String repo_name;
    public String old_path;
    public long size;
    public String time;

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
