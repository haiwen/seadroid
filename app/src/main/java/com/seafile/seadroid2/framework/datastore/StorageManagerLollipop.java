package com.seafile.seadroid2.framework.datastore;

import android.os.StatFs;

import java.io.File;

/**
 * StorageManager implementation for Lollipop or newer devices.
 */
public class StorageManagerLollipop extends StorageManager {

    @Override
    protected File[] getDefaultMediaCacheDirs() {
        /*
         * Since Lollipop there is a proper media directory on every storage device.
         * It is indexed by the gallery and the best place for Seafile to store cached files.
         */
        return getContext().getExternalMediaDirs();
    }

    @Override
    protected File[] getDefaultAppCacheDir() {
        return getContext().getExternalCacheDirs();
    }

    @Override
    protected long getStorageSize(File dir) {
        StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
        return stat.getTotalBytes();
    }

    @Override
    protected long getStorageFreeSpace(File dir) {
        try {
            StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
            return stat.getAvailableBytes();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    @Override
    public boolean supportsMultipleStorageLocations() {
        return true;
    }
}
