package com.seafile.seadroid2.ui.selector.folder_selector;


import com.blankj.utilcode.util.FileUtils;
import com.google.android.material.checkbox.MaterialCheckBox;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.FileTools;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Utils;

import java.io.Serializable;


public class FileBean implements Serializable {
    private String filePath;
    private boolean isDir;

    private String fileName;
    private String fileExtension;
    private int fileImgType;
    private int childrenFileNumber;
    private int childrenDirNumber;
    private String size;


    @MaterialCheckBox.CheckedState
    private int checkedState;

    private long modifyTime;
    private long simpleSize;

    public FileBean(String filePath) {
        this.filePath = filePath;

        isDir = FileUtils.isDir(filePath);

        fileName = FileTools.getFileName(filePath);
        fileExtension = FileTools.getFileExtension(filePath);

        if (isDir) {
            fileImgType = R.drawable.folders;
        } else {
            fileImgType = Icons.getFileIcon(fileName);
        }

        int[] n = FileTools.getChildrenNumber(filePath);
        childrenFileNumber = n[0];
        childrenDirNumber = n[1];
        modifyTime = FileTools.getFileLastModified(filePath);
        if (!isDir) {
            size = FileTools.getSize(filePath);
            simpleSize = FileTools.getSimpleSize(filePath);
        }
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public void setCheckedState(int checkedState) {
        this.checkedState = checkedState;
    }

    public int getCheckedState() {
        return checkedState;
    }

    public String getFilePath() {
        return filePath;
    }

    public boolean isDir() {
        return isDir;
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

    public String getChildrenFileNumber() {
        return String.valueOf(childrenFileNumber);
    }

    public String getChildrenDirNumber() {
        return String.valueOf(childrenDirNumber);
    }

    public String getSize() {
        return size;
    }

    public long getModifyTime() {
        return modifyTime;
    }

    public long getSimpleSize() {
        return simpleSize;
    }

    public void setSize(String size) {
        this.size = size;
    }

    public void setSimpleSize(long simpleSize) {
        this.simpleSize = simpleSize;
    }
}
