package com.seafile.seadroid2.framework.model;

import androidx.annotation.StringRes;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.db.entities.RepoModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

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

    public GroupItemModel(@StringRes int nameRes) {
        this.name = nameRes;
        checkable = false;
        title = SeadroidApplication.getInstance().getString(name);
    }

    public GroupItemModel(@StringRes int nameRes, List<RepoModel> repoList) {
        this.name = nameRes;
        repo_list.addAll(repoList);
        checkable = false;
        title = SeadroidApplication.getInstance().getString(name);
    }

    public GroupItemModel(String title) {
        this.title = title;
        checkable = false;
    }

    public GroupItemModel(String title, List<RepoModel> repoList) {
        this.title = title;
        repo_list.addAll(repoList);
        checkable = false;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GroupItemModel that = (GroupItemModel) o;
        return Objects.equals(title, that.title);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(title);
    }
}
