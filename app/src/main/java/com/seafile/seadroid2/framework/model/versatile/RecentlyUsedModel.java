package com.seafile.seadroid2.framework.model.versatile;

import java.util.Objects;

public class RecentlyUsedModel {
    public String account;
    public String repoId;
    public String repoName;
    public String path;
    public boolean isSelected;

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        RecentlyUsedModel that = (RecentlyUsedModel) o;
        return Objects.equals(account, that.account) && Objects.equals(repoId, that.repoId) && Objects.equals(repoName, that.repoName) && Objects.equals(path, that.path);
    }

    @Override
    public int hashCode() {
        return Objects.hash(account, repoId, repoName, path);
    }
}
