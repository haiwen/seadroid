package com.seafile.seadroid2.enums;

public enum FeatureDataSource {
    ALBUM_BACKUP,
    FOLDER_BACKUP,
    DOWNLOAD,
    MANUAL_FILE_UPLOAD,
    SHARE_FILE_TO_SEAFILE,
    AUTO_UPDATE_LOCAL_FILE;

    public static TransferDataSource toTransferDataSource(FeatureDataSource transferDataSource) {
        switch (transferDataSource) {
            case ALBUM_BACKUP:
                return TransferDataSource.ALBUM_BACKUP;
            case FOLDER_BACKUP:
                return TransferDataSource.FOLDER_BACKUP;
            case DOWNLOAD, AUTO_UPDATE_LOCAL_FILE:
                return TransferDataSource.DOWNLOAD;
            case MANUAL_FILE_UPLOAD, SHARE_FILE_TO_SEAFILE:
                return TransferDataSource.FILE_BACKUP;
            default:
                return null;
        }
    }
}
