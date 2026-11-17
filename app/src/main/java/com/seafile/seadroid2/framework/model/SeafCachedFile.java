package com.seafile.seadroid2.framework.model;

import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Utils;
import com.google.gson.annotations.SerializedName;

import java.io.File;

public class SeafCachedFile implements SeafItem {
    @SerializedName("id")
    public int id;
    @SerializedName("fileID")
    public String fileID;
    @SerializedName("repoName")
    public String repoName;
    @SerializedName("repoID")
    public String repoID;
    @SerializedName("path")
    public String path;
    @SerializedName("accountSignature")
    public String accountSignature;
    @SerializedName("fileOriginalSize")
    public long fileOriginalSize;
    @SerializedName("file")
    protected File file;

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
        return Icons.getFileIcon(file.getName());
    }

    public long getSize() {
        return file.length();
    }

    public long getLastModified() {
        return file.lastModified();
    }

    public boolean isDirectory() {
        return file.isDirectory();
    }

    public String getAccountSignature() {
        return accountSignature;
    }

    public long getFileOriginalSize() {
        return fileOriginalSize;
    }

    public void setFileOriginalSize(long fileOriginalSize) {
        this.fileOriginalSize = fileOriginalSize;
    }
}
