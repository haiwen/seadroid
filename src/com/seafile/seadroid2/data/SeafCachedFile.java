package com.seafile.seadroid2.data;

import java.io.File;

import com.seafile.seadroid2.Utils;

public class SeafCachedFile implements SeafItem {
    
    public int id;
    public String fileID;
    public String repoName;
    public String repoID;
    public String path;
    public String accountSignature;
    File file;

    public SeafCachedFile() {
        id = -1;
    }
    
    @Override
    public String getTitle() {
        return path.substring(path.lastIndexOf('/') + 1);
    }

    @Override
    public String getSubtitle() {
        return Utils.readableFileSize(file.length());
    }

    @Override
    public int getIcon() {
        return Utils.getFileIcon(file.getName());
    }
    
    public long getSize() {
        return file.length();
    }
    
    public String getAccountSignature() {
        return accountSignature;
    }
}
