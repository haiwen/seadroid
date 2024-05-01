package com.seafile.seadroid2.ui.folder_backup;

public class FolderBackupEvent {
    private String backupInfo;

    public FolderBackupEvent(String backupInfo) {
        this.backupInfo = backupInfo;
    }

    public String getBackupInfo() {
        return backupInfo;
    }
}
