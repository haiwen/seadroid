package com.seafile.seadroid2.cameraupload;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;

import com.blankj.utilcode.util.TimeUtils;

import java.util.Timer;
import java.util.TimerTask;

/**
 * Camera Sync Service.
 * <p>
 * This service is started and stopped by the Android System.
 */
public class CameraSyncService extends Service {

    private static CameraSyncAdapter sSyncAdapter = null;
    private static final Object sSyncAdapterLock = new Object();

    @Override
    public void onCreate() {
//        Log.i(CameraSyncService.class.getName(), "CameraSyncService onCreate");
        synchronized (sSyncAdapterLock) {
            if (sSyncAdapter == null) {
                sSyncAdapter = new CameraSyncAdapter(getApplicationContext());
            }
        }
    }

    @Override
    public void onDestroy() {
//        Log.i(CameraSyncService.class.getName(), "CameraSyncService onDestroy");
        super.onDestroy();
    }

    @Override
    public IBinder onBind(Intent intent) {
//        Log.i(CameraSyncService.class.getName(), "CameraSyncService onBind");
        return sSyncAdapter.getSyncAdapterBinder();
    }
}
