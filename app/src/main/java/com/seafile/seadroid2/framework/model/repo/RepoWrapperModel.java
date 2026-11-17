package com.seafile.seadroid2.framework.model.repo;

import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class RepoWrapperModel {
    @SerializedName("repos")
    public List<RepoModel> repos;
}
