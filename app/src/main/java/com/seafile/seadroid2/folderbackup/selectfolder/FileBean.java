package com.seafile.seadroid2.folderbackup.selectfolder;


import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.Utils;

import java.io.Serializable;


public class FileBean implements Serializable {
    private String filePath;
    private boolean dir;
    private boolean file;
    private String fileName;
    private String fileExtension;
    private int fileImgType;
    private String parentPath;
    private String parentName;
    private int childrenFileNumber;
    private int childrenDirNumber;
    private String size;
    private boolean visible;
    private boolean checked;
    private long modifyTime;
    private long simpleSize;

    public FileBean(String filePath) {
        this.filePath = filePath;
        visible = false;
        checked = false;
        if (FileTools.isFile(filePath)) {
            file = true;
            dir = false;
        } else {
            file = false;
            dir = true;
        }
        fileName = FileTools.getFileName(filePath);
        fileExtension = FileTools.getFileExtension(filePath);
        fileImgType = setImageResourceByExtension(fileExtension);
        parentPath = FileTools.getParentPath(filePath);
        parentName = FileTools.getDirName(filePath);
        childrenFileNumber = FileTools.getChildrenNumber(filePath)[0];
        childrenDirNumber = FileTools.getChildrenNumber(filePath)[1];
        modifyTime = FileTools.getFileLastModified(filePath);
        if (file) {
            size = FileTools.getSize(filePath);
            simpleSize = FileTools.getSimpleSize(filePath);
        }
    }

    public int setImageResourceByExtension(String extension) {
        int resourceId;
        switch (extension) {
            default:
                if (dir) {
                    resourceId = R.drawable.folder;
                } else {
                    resourceId = Utils.getFileIconSuffix(extension);
                }
                break;
        }
        return resourceId;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    public void setChecked(boolean checked) {
        this.checked = checked;
    }

    public String getFilePath() {
        return filePath;
    }

    public boolean isDir() {
        return dir;
    }

    public boolean isFile() {
        return file;
    }

    public String getFileName() {
        return fileName;
    }

    public String getFileExtension() {
        return fileExtension;
    }

    public int getFileImgType() {
        return fileImgType;
    }

    public String getParentPath() {
        return parentPath;
    }

    public String getParentName() {
        return parentName;
    }

    public int getChildrenFileNumber() {
        return childrenFileNumber;
    }

    public int getChildrenDirNumber() {
        return childrenDirNumber;
    }

    public String getSize() {
        return size;
    }

    public boolean isVisible() {
        return visible;
    }

    public boolean isChecked() {
        return checked;
    }

    public long getModifyTime() {
        return modifyTime;
    }

    public long getSimpleSize() {
        return simpleSize;
    }

    public void setIsDir(Boolean dir) {
        this.dir = dir;
    }

    public void setIsFile(Boolean file) {
        this.file = file;
    }

    public void setSize(String size) {
        this.size = size;
    }

    public void setSimpleSize(long simpleSize) {
        this.simpleSize = simpleSize;
    }
}
