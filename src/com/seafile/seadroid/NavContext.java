package com.seafile.seadroid;

import java.util.List;

public class NavContext {

    // for repos fragment
    String currentRepo = null;
    List<SeafDirent> currentDirents = null;
    String currentPath = null;
    String currentObjectID = null;
    
    // for file fragment
    String currentFilePath = null;
    SeafDirent currentDirent = null;
    int position = 0;
    
    public NavContext() {
        currentRepo = null;
        currentDirents = null;
        currentPath = null;
    }
 
    public void clear() {
        currentRepo = null;
        currentDirents = null;
        currentPath = null;
    }
    
    public boolean inRepo() {
        return currentRepo != null;
    }
    
    public String getCurrentRepoID() {
        return currentRepo;
    }
    
    public SeafDirent getDirent(int position) {
        return currentDirents.get(position);
    }
    
    public boolean isDir(int position) {
        return currentDirents.get(position).isDir();
    }
    
    public String getPathAtPosition(int position) {
        if (isRootDir())
            return currentPath + currentDirents.get(position).name;
        else
            return currentPath + "/" + currentDirents.get(position).name;
    }
    
    public boolean isRootDir() {
        return currentPath.equals("/");
    }
    
    public String getParentPath() {
        return currentPath.substring(0, currentPath.lastIndexOf("/"));
    }
    
    public String getCurrentPath() {
        return currentPath;
    }
    
}
