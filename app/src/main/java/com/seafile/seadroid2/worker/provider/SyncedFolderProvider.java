package com.seafile.seadroid2.worker.provider;

import android.content.ContentResolver;
import android.database.Cursor;

public class SyncedFolderProvider {
    private final ContentResolver mContentResolver;

    public SyncedFolderProvider(ContentResolver mContentResolver) {
        this.mContentResolver = mContentResolver;
    }

    /**
     * Stores a synced folder object in database.
     *
     * @return synced folder id, -1 if the insert process fails.
     */
    public long storeSyncedFolder() {
        return 0;
    }


    public int countEnabledSyncedFolders() {
        int count = 0;
        return count;
    }
}
