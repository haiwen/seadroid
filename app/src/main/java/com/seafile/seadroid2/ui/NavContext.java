package com.seafile.seadroid2.ui;

public class NavContext {
    String repoID = null;
    String repoName = null;     // for display
    String dirPath = null;
    String dirID = null;
    String dirPermission = null;

    public NavContext() {
    }

    public void setRepoID(String repoID) {
        this.repoID = repoID;
    }

    public void setRepoName(String repoName) {
        this.repoName = repoName;
    }

    public void setDir(String path, String dirID) {
        this.dirPath = path;
        this.dirID = dirID;
    }

    public void setDirID(String dirID) {
        this.dirID = dirID;
    }

    public boolean inRepo() {
        return repoID != null;
    }

    public String getRepoID() {
        return repoID;
    }

    public String getRepoName() {
        return repoName;
    }

    public boolean isRepoRoot() {
        return "/".equals(dirPath);
    }

    public String getDirPath() {
        return dirPath;
    }

    public String getDirID() {
        return dirID;
    }

    public String getDirPermission() {
        return dirPermission;
    }

    public void setDirPermission(String dirPermission) {
        this.dirPermission = dirPermission;
    }
}
