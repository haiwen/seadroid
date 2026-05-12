package com.seafile.seadroid2.framework.model.activities;

import android.text.TextUtils;

import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class ActivityModel extends BaseModel {

    public String author_contact_email;
    public String author_email;
    public String author_name;
    public String avatar_url;
    public String commit_id;
    public String login_id;
    public String name;
    public String op_type;
    public String obj_type;
    public String repo_id;
    public String path;
    public String repo_name;
    public String time;
    public String old_path;
    public String old_name;


    // custom
    public String related_account;

    // server version >= 14.0
    public int count = 0;
    public List<ActivityDetailModel> details;

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

    public boolean isDir() {
        return TextUtils.equals(obj_type, "dir");
    }
}
