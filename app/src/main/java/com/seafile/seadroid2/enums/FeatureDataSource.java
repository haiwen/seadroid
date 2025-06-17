package com.seafile.seadroid2.enums;

public enum FeatureDataSource {
    ALBUM_BACKUP,
    FOLDER_BACKUP,
    DOWNLOAD,
    MANUAL_FILE_UPLOAD,
    SHARE_FILE_TO_SEAFILE,
    AUTO_UPDATE_LOCAL_FILE;

    public static TransferDataSource toTransferDataSource(FeatureDataSource transferDataSource) {
        return switch (transferDataSource) {
            case ALBUM_BACKUP -> TransferDataSource.ALBUM_BACKUP;
            case FOLDER_BACKUP -> TransferDataSource.FOLDER_BACKUP;
            case DOWNLOAD, AUTO_UPDATE_LOCAL_FILE -> TransferDataSource.DOWNLOAD;
            case MANUAL_FILE_UPLOAD, SHARE_FILE_TO_SEAFILE -> TransferDataSource.FILE_BACKUP;
        };
    }
}
