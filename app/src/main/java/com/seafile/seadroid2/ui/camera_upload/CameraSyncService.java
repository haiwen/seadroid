package com.seafile.seadroid2.ui.camera_upload;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

/**
 * Camera Sync Service.
 * <p>
 * This service is started and stopped by the Android System.
 */
public class CameraSyncService extends Service {

    private static CameraSyncAdapter sSyncAdapter = null;
    private static  AlbumBackupAdapter albumBackupAdapter;
    private static final Object sSyncAdapterLock = new Object();

    @Override
    public void onCreate() {
        synchronized (sSyncAdapterLock) {
//            if (sSyncAdapter == null) {
//                sSyncAdapter = new CameraSyncAdapter(getApplicationContext());
//            }

            if (albumBackupAdapter == null) {
                albumBackupAdapter = new AlbumBackupAdapter(getApplicationContext());
            }
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        return albumBackupAdapter.getSyncAdapterBinder();
    }
}
