package com.seafile.seadroid2.framework.data.model.objs;

import com.seafile.seadroid2.framework.data.model.dirents.DirentPermissionModel;

public class DirentShareLinkModel {
    public String username;
    public String repo_id;
    public String repo_name;
    public String path;
    public String password;
    public String obj_name;
    public boolean is_dir;
    public String token;
    public String link;
    public int view_count;
    public String ctime;
    public String expire_date;
    public boolean is_expired;
    public DirentPermissionModel permissions;

}
