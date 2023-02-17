package com.seafile.seadroid2.folderbackup.selectfolder;


public class TabbarFileBean {

    private String filePath;
    private String fileName;
    private String fileNameNoExtension;
    private String parentPath;
    private String parentName;

    public TabbarFileBean(String filePath) {
        this.filePath = filePath;
        fileName = FileTools.getFileName(filePath);
        fileNameNoExtension = FileTools.getFileNameNoExtension(filePath);
        parentPath = FileTools.getParentPath(filePath);
        parentName = FileTools.getDirName(filePath);
    }

    public TabbarFileBean(String filePath, String fileName) {
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
