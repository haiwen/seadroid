package com.seafile.seadroid2.backupdirectory;


import android.support.v4.provider.DocumentFile;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.io.Serializable;


public class FileBean implements Serializable {
    private String filePath;
    private boolean dir;
    private boolean file;
    private String fileName;
    private String fileNameNoExtension;
    private String fileExtension;
    private int fileImgType;
    private String parentPath;
    private String parentName;
    private int childrenFileNumber;
    private int childrenDirNumber;
    private String size;
    private boolean visible;
    private boolean checked;

    private boolean useUri;
    private DocumentFile documentFile;

    private long modifyTime;
    private long simpleSize;


    public FileBean(String filePath, Boolean useUri) {
        this(filePath, useUri, null);
    }

    public FileBean(String filePath, Boolean useUri, DocumentFile documentFile) {
        this.filePath = filePath;
        this.useUri = useUri;
        visible = false;
        checked = false;

        if (this.useUri) {
            this.documentFile = documentFile;
            File uriFile = UriTools.uri2File(documentFile.getUri(), Commons.getApplicationByReflect().getBaseContext(), Commons.getApplicationByReflect());

            if (documentFile.isFile()) {
                file = true;
                dir = false;
            } else {
                file = false;
                dir = true;
            }
            fileName = FileTools.getFileName(uriFile);
            fileNameNoExtension = FileTools.getFileNameNoExtension(uriFile);
            fileExtension = FileTools.getFileExtension(uriFile);
            fileImgType = setImageResourceByExtension(fileExtension);
            parentPath = null;
            parentName = FileTools.getDirName(uriFile);
            childrenFileNumber = 0;
            childrenDirNumber = 0;

            modifyTime = documentFile.lastModified();
            if (file) {
                size = FileTools.getSize(uriFile);
                simpleSize = FileTools.getSimpleSize(uriFile);
            }
        } else {

            if (FileTools.isFile(filePath)) {
                file = true;
                dir = false;
            } else {
                file = false;
                dir = true;
            }
            fileName = FileTools.getFileName(filePath);
            fileNameNoExtension = FileTools.getFileNameNoExtension(filePath);
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

    public void setUseUri(boolean useUri) {
        this.useUri = useUri;
    }

    public void setDocumentFile(DocumentFile documentFile) {
        this.documentFile = documentFile;
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

    public String getFileNameNoExtension() {
        return fileNameNoExtension;
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

    public boolean isUseUri() {
        return useUri;
    }

    public DocumentFile getDocumentFile() {
        return documentFile;
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
