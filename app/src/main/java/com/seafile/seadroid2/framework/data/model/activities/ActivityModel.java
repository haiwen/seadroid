package com.seafile.seadroid2.framework.data.model.activities;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;

public class ActivityModel extends BaseModel {
    public String op_type;
    public String related_account;
    public String repo_id;
    public String repo_name;
    public String obj_type;
    public String commit_id;
    public String path;
    public String name;
    public String old_path;
    public String old_name;
    public String author_email;
    public String author_name;
    public String author_contact_email;
    public String avatar_url;
    public String time;

    private long mTimeLong;
    public OpType opType;

    public String getTime() {
        if (mTimeLong == 0) {
            mTimeLong = Times.convertMtime2Long(time);
        }
        return Utils.translateCommitTime(mTimeLong);
    }


    public boolean isFileOpenable() {
        return opType == OpType.CREATE ||
                opType == OpType.UPDATE ||
                opType == OpType.RENAME ||
                opType == OpType.EDIT;
    }

    public boolean isDir() {
        return TextUtils.equals(obj_type, "dir");
    }
}
