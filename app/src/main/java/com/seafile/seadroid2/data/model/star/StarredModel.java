package com.seafile.seadroid2.data.model.star;

import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.util.Times;
import com.seafile.seadroid2.util.Utils;

public class StarredModel extends BaseModel {
    public String repo_id;
    public String repo_name;
    public boolean repo_encrypted;
    public boolean is_dir;
    public boolean deleted;

    public String mtime;
    public String path;
    public String obj_name;
    public String user_email;
    public String user_name;
    public String user_contact_email;

    //image thumbnail url
    public String encoded_thumbnail_src;
    public long size;
//    public FileType type;

    private long mTimeLong;

    public String getSubtitle() {
        if (deleted) {
            return repo_name;
        }

        if (TextUtils.isEmpty(mtime)) {
            return repo_name;
        }

        if (mTimeLong == 0) {
            mTimeLong = Times.convertMtime2Long(mtime);
        }

        return repo_name + " " + Utils.translateCommitTime(mTimeLong);
    }

    public boolean isRepo() {
        return TextUtils.equals("/", path) && is_dir;
    }

}
