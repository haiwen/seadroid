package com.seafile.seadroid2.framework.model.activities;

import android.text.TextUtils;

import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;
import com.google.gson.annotations.SerializedName;

import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class ActivityModel extends BaseModel {

    @SerializedName("author_contact_email")
    public String author_contact_email;
    @SerializedName("author_email")
    public String author_email;
    @SerializedName("author_name")
    public String author_name;
    @SerializedName("avatar_url")
    public String avatar_url;
    @SerializedName("commit_id")
    public String commit_id;
    @SerializedName("login_id")
    public String login_id;
    @SerializedName("name")
    public String name;
    @SerializedName("op_type")
    public String op_type;
    @SerializedName("obj_type")
    public String obj_type;
    @SerializedName("repo_id")
    public String repo_id;
    @SerializedName("path")
    public String path;
    @SerializedName("repo_name")
    public String repo_name;
    @SerializedName("time")
    public String time;
    @SerializedName("old_path")
    public String old_path;
    @SerializedName("old_name")
    public String old_name;


    // custom
    @SerializedName("related_account")
    public String related_account;

    // server version >= 14.0
    @SerializedName("count")
    public int count = 0;
    @SerializedName("details")
    public List<ActivityDetailModel> details;

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

    public boolean isDir() {
        return TextUtils.equals(obj_type, "dir");
    }
}
