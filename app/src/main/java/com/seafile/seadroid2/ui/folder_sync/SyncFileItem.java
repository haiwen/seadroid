package com.seafile.seadroid2.ui.folder_sync;

/**
 * Represents a single remote file in the per-file toggle list.
 */
public class SyncFileItem {
    public final String relativePath;
    public final String fileName;
    public final long fileSize;
    public boolean included;

    public SyncFileItem(String relativePath, String fileName, long fileSize, boolean included) {
        this.relativePath = relativePath;
        this.fileName = fileName;
        this.fileSize = fileSize;
        this.included = included;
    }
}
