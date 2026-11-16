package com.seafile.seadroid2.ui.camera_upload;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

import com.seafile.seadroid2.framework.util.SLogs;

/**
 * The synchronization entry service bound to the Android system does not manage the lifecycle of album upload tasks.
 */
public class CameraSyncService extends Service {
    private static AlbumBackupAdapter albumBackupAdapter;
    private static final Object sSyncAdapterLock = new Object();

    @Override
    public void onCreate() {
        SLogs.d("CameraSyncService", "onCreate");
        synchronized (sSyncAdapterLock) {
            if (albumBackupAdapter == null) {
                albumBackupAdapter = new AlbumBackupAdapter(getApplicationContext());
            }
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        SLogs.d("CameraSyncService", "onBind");
        return albumBackupAdapter.getSyncAdapterBinder();
    }
}
