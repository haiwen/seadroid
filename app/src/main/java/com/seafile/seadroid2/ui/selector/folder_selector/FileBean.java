package com.seafile.seadroid2.ui.selector.folder_selector;


import com.google.android.material.checkbox.MaterialCheckBox;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.FileTools;
import com.seafile.seadroid2.framework.util.Icons;

import java.io.File;
import java.io.Serializable;


public class FileBean implements Serializable {
    private File file;
    private int childrenFileNumber;
    private int childrenDirNumber;
    private String size;

    @MaterialCheckBox.CheckedState
    private int checkedState;

    private long modifyTime;
    private long simpleSize;

    public FileBean(File file) {
        if (file == null) {
            throw new NullPointerException("file is null");
        }
        this.file = file;

        int[] n = FileTools.getChildrenNumber(file);
        childrenFileNumber = n[0];
        childrenDirNumber = n[1];
        modifyTime = FileTools.getFileLastModified(file);
        if (!file.isDirectory()) {
            size = FileTools.getSize(file);
            simpleSize = FileTools.getSimpleSize(file);
        }
    }


    public void setCheckedState(int checkedState) {
        this.checkedState = checkedState;
    }

    public int getCheckedState() {
        return checkedState;
    }

    public String getFilePath() {
        return file.getAbsolutePath();
    }

    public boolean isDir() {
        return file.isDirectory();
    }

    public String getFileName() {
        return file.getName();
    }

    public int getFileImgType() {
        if (isDir()) {
            return R.drawable.folders;
        } else {
            return Icons.getFileIcon(getFileName());
        }
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
