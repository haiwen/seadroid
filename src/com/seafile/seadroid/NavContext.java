package com.seafile.seadroid;

import java.util.List;

import com.seafile.seadroid.data.SeafDirent;

public class NavContext {
    
    String repoID = null;
    String repoName = null;     // for display
    String dirPath = null;
    String dirID = null;
    List<SeafDirent> dirents = null;

    
    public NavContext() {
        repoID = null;
        dirPath = null;
        dirents = null;
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
        return dirPath.equals("/");
    }
    
    public String getDirPath() {
        return dirPath;
    }
    
    public String getDirID() {
        return dirID;
    }


}
