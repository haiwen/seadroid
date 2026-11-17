package com.seafile.seadroid2.framework.notification;

import com.seafile.seadroid2.enums.FeatureDataSource;

/**
 * Stable business identities. Never derive IDs from ordinal or a file ID.
 */
public enum NotificationKey {
    MANUAL(3300, FeatureDataSource.MANUAL_FILE_UPLOAD),
    ALBUM(3100, FeatureDataSource.ALBUM_BACKUP),
    FOLDER(3201, FeatureDataSource.FOLDER_BACKUP),
    SHARE(3302, FeatureDataSource.SHARE_FILE_TO_SEAFILE),
    LOCAL_UPDATE(3301, FeatureDataSource.AUTO_UPDATE_LOCAL_FILE),
    DOWNLOAD(3400, FeatureDataSource.DOWNLOAD);

    public final int id;
    public final FeatureDataSource source;

    NotificationKey(int id, FeatureDataSource source) {
        this.id = id;
        this.source = source;
    }

    public static NotificationKey from(FeatureDataSource source) {
        for (NotificationKey key : values()) if (key.source == source) return key;
        throw new IllegalArgumentException("Unknown transfer source");
    }
}
