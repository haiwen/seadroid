package com.seafile.seadroid2.data;

import android.annotation.TargetApi;
import android.os.Build;
import android.os.StatFs;

import java.io.File;

/**
 * StorageManager implementation for Lollipop or newer devices.
 */
@TargetApi(Build.VERSION_CODES.LOLLIPOP)
public class StorageManagerLollipop extends StorageManager {

    @Override
    protected File[] getSystemMediaDirs() {
        /*
         * Since Lollipop there is a proper media directory on every storage device.
         * It is indexed by the gallery and the best place for Seafile to store cached files.
         */
        return getContext().getExternalMediaDirs();
    }

    @Override
    protected File[] getSystemCacheDirs() {
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
