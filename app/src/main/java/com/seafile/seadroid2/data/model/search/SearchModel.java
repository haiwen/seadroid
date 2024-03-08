package com.seafile.seadroid2.data.model.search;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.util.Utils;

public class SearchModel extends BaseModel {

    public boolean is_dir;
    public String fullpath;
    public String repo_id;
    public String repo_name;
    public String repo_owner_email;
    public String repo_owner_name;
    public String repo_owner_contact_email;
    public String thumbnail_url;
    public String repo_type;

    public String name;
    public String content_highlight;

    public long last_modified;
    public long size;    // size of file, 0 if type is dir


    public String getTitle() {
        return name;
    }

    //    public String getSubtitle() {
//        String timestamp = Utils.translateCommitTime(last_modified * 1000);
//        if (is_dir)
//            return timestamp;
//        return Utils.readableFileSize(size) + ", " + timestamp;
//    }

    public String getSubtitle() {
        return fullpath;
    }


    public int getIcon() {
        if (is_dir)
            return R.drawable.folder;
        return Utils.getFileIcon(getTitle());
    }
}
