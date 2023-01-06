package com.seafile.seadroid2.backupdirectory;


import android.support.v4.provider.DocumentFile;

import java.io.File;


public class TabbarFileBean {
    private String filePath;
    private String fileName;
    private String fileNameNoExtension;
    private String parentPath;
    private String parentName;

    private Boolean useUri;
    private DocumentFile documentFile;//documentFile

    public TabbarFileBean(String filePath, Boolean useUri) {
        this(filePath, useUri, null);
    }

    public TabbarFileBean(String filePath, Boolean useUri, DocumentFile documentFile) {
        this.filePath = filePath;
        this.useUri = useUri;

        if (PermissionsTools.isAndroid11()) {
            this.useUri = useUri;
        } else {
            this.useUri = false;
        }

        if (this.useUri) {
            this.documentFile = documentFile;
            File uriFile = UriTools.uri2File(documentFile.getUri(), Commons.getApplicationByReflect().getBaseContext(), Commons.getApplicationByReflect());
            fileName = FileTools.getFileName(uriFile);
            fileNameNoExtension = FileTools.getFileNameNoExtension(uriFile);
            parentPath = null;
            parentName = FileTools.getDirName(uriFile);

        } else {
            fileName = FileTools.getFileName(filePath);
            fileNameNoExtension = FileTools.getFileNameNoExtension(filePath);
            parentPath = FileTools.getParentPath(filePath);
            parentName = FileTools.getDirName(filePath);
        }


    }


    public TabbarFileBean(String filePath, String fileName, Boolean useUri, DocumentFile documentFile) {
        this.filePath = filePath;
        this.useUri = useUri;
        this.fileName = fileName;

        if (this.useUri) {
            this.documentFile = documentFile;
            File uriFile = UriTools.uri2File(documentFile.getUri(), Commons.getApplicationByReflect().getBaseContext(), Commons.getApplicationByReflect());
            fileNameNoExtension = FileTools.getFileNameNoExtension(uriFile);
            parentPath = null;
            parentName = FileTools.getDirName(uriFile);

        } else {

            fileNameNoExtension = fileName;
            parentPath = FileTools.getParentPath(filePath);
            parentName = FileTools.getDirName(filePath);

        }

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
