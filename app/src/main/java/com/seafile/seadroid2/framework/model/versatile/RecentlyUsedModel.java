package com.seafile.seadroid2.framework.model.versatile;

import java.util.Objects;
import com.google.gson.annotations.SerializedName;

public class RecentlyUsedModel {
    @SerializedName("account")
    public String account;
    @SerializedName("repoId")
    public String repoId;
    @SerializedName("repoName")
    public String repoName;
    @SerializedName("path")
    public String path;
    @SerializedName("isSelected")
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
