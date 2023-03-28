package com.seafile.seadroid2.folderbackup;

public class FolderBackupEvent {
    private String backupInfo;

    public FolderBackupEvent(String backupInfo) {
        this.backupInfo = backupInfo;
    }

    public String getBackupInfo() {
        return backupInfo;
    }
}
