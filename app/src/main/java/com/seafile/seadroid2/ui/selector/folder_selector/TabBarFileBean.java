package com.seafile.seadroid2.ui.selector.folder_selector;


import com.seafile.seadroid2.util.FileTools;

public class TabBarFileBean {

    private String filePath;
    private String fileName;
    private String fileNameNoExtension;
    private String parentPath;
    private String parentName;

    public TabBarFileBean(String filePath) {
        this.filePath = filePath;
        fileName = FileTools.getFileName(filePath);
        fileNameNoExtension = FileTools.getFileNameNoExtension(filePath);
        parentPath = FileTools.getParentPath(filePath);
        parentName = FileTools.getDirName(filePath);
    }

    public TabBarFileBean(String filePath, String fileName) {
        this.filePath = filePath;
        this.fileName = fileName;
        fileNameNoExtension = fileName;
        parentPath = FileTools.getParentPath(filePath);
        parentName = FileTools.getDirName(filePath);
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public String getFilePath() {
        return filePath;
    }

    public String getFileName() {
        return fileName;
    }

    public String getParentPath() {
        return parentPath;
    }
}
