package com.seafile.seadroid2.cameraupload;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

/**
 * Camera Sync Service.
 *
 * This service is started and stopped by the Android System.
 */
public class CameraSyncService extends Service {

    private static CameraSyncAdapter sSyncAdapter = null;
    private static final Object sSyncAdapterLock = new Object();

    @Override
    public void onCreate() {
        synchronized (sSyncAdapterLock) {
            if (sSyncAdapter == null) {
                sSyncAdapter = new CameraSyncAdapter(getApplicationContext());
            }
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        return sSyncAdapter.getSyncAdapterBinder();
    }

}
