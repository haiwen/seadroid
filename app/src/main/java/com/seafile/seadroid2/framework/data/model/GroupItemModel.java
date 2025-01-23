package com.seafile.seadroid2.framework.data.model;

import androidx.annotation.StringRes;
import androidx.room.Ignore;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;

import java.util.ArrayList;
import java.util.List;

public class GroupItemModel extends BaseModel {
    @StringRes
    private int name;

    public String title;

    public boolean is_expanded = true;

    //
    public final List<RepoModel> repo_list = new ArrayList<>();

    public void addAllRepoList(List<RepoModel> repoList) {
        repo_list.addAll(repoList);
    }

    public List<RepoModel> getRepoList() {
        return repo_list;
    }

    public void clearRepoList() {
        repo_list.clear();
    }

    public GroupItemModel() {

    }

    public GroupItemModel(@StringRes int nameRes) {
        this.name = nameRes;

        title = SeadroidApplication.getInstance().getString(name);
    }

    public GroupItemModel(@StringRes int nameRes, List<RepoModel> repoList) {
        this.name = nameRes;
        repo_list.addAll(repoList);

        title = SeadroidApplication.getInstance().getString(name);
    }

    public GroupItemModel(String title) {
        this.title = title;
    }

    public GroupItemModel(String title, List<RepoModel> repoList) {
        this.title = title;
        repo_list.addAll(repoList);
    }
}
