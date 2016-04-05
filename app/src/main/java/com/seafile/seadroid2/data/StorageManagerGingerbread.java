package com.seafile.seadroid2.data;

import android.os.StatFs;

import java.io.File;

/**
 * StorageManager implementation for pre-KitKat devices.
 */
public class StorageManagerGingerbread extends StorageManager {

    @Override
    protected File[] getSystemMediaDirs() {
        return new File[]{};
    }

    @Override
    protected File[] getSystemCacheDirs() {
        return new File[]{};
    }

    @Override
    protected long getStorageSize(File dir) {
        StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
        return (long)stat.getBlockCount() * stat.getBlockSize();
    }

    @Override
    protected long getStorageFreeSpace(File dir) {
        StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
        return (long)stat.getAvailableBlocks() * stat.getBlockSize();
    }

    @Override
    public boolean supportsMultipleStorageLocations() {
        return false;
    }
}
