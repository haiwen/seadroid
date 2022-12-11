package com.seafile.seadroid2.cameraupload;

import android.app.Service;
import android.content.ContentResolver;
import android.content.Intent;
import android.os.Bundle;
import android.os.IBinder;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;

/**
 * Camera Sync Service.
 *
 * This service is started and stopped by the Android System.
 */
public class CameraSyncService extends Service {

    private static CameraSyncAdapter sSyncAdapter = null;
    private static final Object sSyncAdapterLock = new Object();

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

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
