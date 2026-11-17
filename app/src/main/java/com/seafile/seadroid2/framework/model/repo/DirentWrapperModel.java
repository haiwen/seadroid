package com.seafile.seadroid2.framework.model.repo;

import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.google.gson.annotations.SerializedName;

import java.util.ArrayList;
import java.util.List;

public class DirentWrapperModel extends ResultModel {
    @SerializedName("user_perm")
    public String user_perm;
    @SerializedName("dir_id")
    public String dir_id;
    @SerializedName("dirent_list")
    public List<DirentModel> dirent_list = new ArrayList<>();
}
