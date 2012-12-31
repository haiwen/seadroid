package com.seafile.seadroid;

import java.util.List;

import com.seafile.seadroid.data.SeafDirent;

public class NavContext {

    boolean inFileView;
    
    String repoID = null;
    String dirPath = null;
    String dirID = null;
    List<SeafDirent> dirents = null;

    private String filePath = null;
    private String fileID = null;
    private long fileSize = 0;
    
    public NavContext() {
        repoID = null;
        dirPath = null;
        dirents = null;
        inFileView = false;
    }
 
    public void setRepo(String repoID) {
        this.repoID = repoID;
    }
    
    public void setDir(String path, String dirID) {
        this.dirPath = path;
        this.dirID = dirID;
    }
    
    public void setFile(String filePath,
            String fileID, long fileSize) {
        this.fileID = fileID;
        this.filePath = filePath;
        this.fileSize = fileSize;
    }
    
    public boolean inRepo() {
        return repoID != null;
    }
    
    public String getRepo() {
        return repoID;
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
    
    public String getFilePath() {
        return filePath;
    }
    
    public String getFileID() {
        return fileID;
    }
    
    public long getFileSize() {
        return fileSize;
    }


}
