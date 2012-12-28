package com.seafile.seadroid;

import java.util.List;

public class NavContext {

    // for repos fragment
    String currentRepo = null;
    String currentDirPath = null;
    List<SeafDirent> currentDirents = null;
    
    // for file fragment
    private String currentFileRepo = null;
    private String currentFilePath = null;
    private String currentFileID = null;
    private long currentFileSize = 0;
    
    public NavContext() {
        currentRepo = null;
        currentDirents = null;
        currentDirPath = null;
    }
 
    public void clear() {
        currentRepo = null;
        currentDirents = null;
        currentDirPath = null;
    }
    
    public boolean inRepo() {
        return currentRepo != null;
    }
    
    public String getCurrentDirRepo() {
        return currentRepo;
    }
    
    public void setCurrentDirRepo(String repoID) {
        currentRepo = repoID;
    }
    
    public SeafDirent getDirent(int position) {
        return currentDirents.get(position);
    }
    
    public boolean isDir(int position) {
        return currentDirents.get(position).isDir();
    }
    
    public String getPathAtPosition(int position) {
        if (isRootDir())
            return currentDirPath + currentDirents.get(position).name;
        else
            return currentDirPath + "/" + currentDirents.get(position).name;
    }
    
    public boolean isRootDir() {
        return currentDirPath.equals("/");
    }
    
    public String getParentPath() {
        String parent = currentDirPath.substring(0, currentDirPath.lastIndexOf("/"));
        if (parent.equals("")) {
            return "/";
        } else
            return parent;
    }
    
    public String getCurrentDirPath() {
        return currentDirPath;
    }
    
    public void setCurrentDir(String path) {
        currentDirPath = path;
    }
    
    
    public String getCurrentFilePath() {
        return currentFilePath;
    }
    
    public String getCurrentFileID() {
        return currentFileID;
    }
    
    public long getCurrentFileSize() {
        return currentFileSize;
    }
    
    public void clearFileNav() {
        currentFileRepo = null;
        currentFilePath = null;
        currentFileID = null;
        currentFileSize = 0;
    }
    
    public String getCurrentFileRepo() {
        return currentFileRepo;
    }
    
    public void setCurrentFileRepo(String repoID) {
        currentFileRepo = repoID;
    }
    
    public void setCurrentFilePath(String path) {
        currentFilePath = path;
    }
    
    public void setCurrentFileID(String fileID) {
        currentFileID = fileID;
    }
    
    public void setCurrentFileSize(long size) {
        currentFileSize = size;
    }

    public String getFileParentPath() {
        String parent = currentFilePath.substring(0, currentFilePath.lastIndexOf("/"));
        if (parent.equals("")) {
            return "/";
        } else
            return parent;
    }
}
