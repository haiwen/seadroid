package com.seafile.seadroid2.framework.file_monitor;

import android.content.Intent;

import androidx.core.content.ContextCompat;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.util.SLogs;

public class FileDaemonServiceManager {
    private static final String TAG = "FileDaemonServiceManager";

    private static volatile FileDaemonServiceManager instance;
    private final Object lock = new Object();

    private FileDaemonServiceManager() {
    }

    public static FileDaemonServiceManager getInstance() {
        if (instance == null) {
            synchronized (FileDaemonServiceManager.class) {
                if (instance == null) {
                    instance = new FileDaemonServiceManager();
                }
            }
        }
        return instance;
    }

    public boolean startService() {
        synchronized (lock) {
            try {
                Intent intent = new Intent(SeadroidApplication.getAppContext(), FileDaemonService.class);
                ContextCompat.startForegroundService(SeadroidApplication.getAppContext(), intent);

                SLogs.d(TAG, "FileDaemonService start requested");
                return true;

            } catch (Exception e) {
                SLogs.e(TAG, "Failed to start FileDaemonService", e);
                return false;
            }
        }
    }

    public void stopService() {
        synchronized (lock) {
            try {
                Intent intent = new Intent(SeadroidApplication.getAppContext(), FileDaemonService.class);
                SeadroidApplication.getAppContext().stopService(intent);
                SLogs.d(TAG, "FileDaemonService stop requested");

            } catch (Exception e) {
                SLogs.e(TAG, "Failed to stop FileDaemonService", e);
            }
        }
    }
}
