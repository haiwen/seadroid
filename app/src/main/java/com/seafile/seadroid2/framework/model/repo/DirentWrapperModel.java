package com.seafile.seadroid2.framework.model.repo;

import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.ResultModel;

import java.util.ArrayList;
import java.util.List;

public class DirentWrapperModel extends ResultModel {
    public String user_perm;
    public String dir_id;
    public List<DirentModel> dirent_list = new ArrayList<>();
}
