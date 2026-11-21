package com.seafile.seadroid2.framework.file_monitor;

import android.content.Intent;
import android.os.Build;

import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.AppUtils;
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
            // On Android 12+ (API 31+), foreground services cannot be started from the background.
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S && !AppUtils.isAppForeground()) {
                SLogs.d(TAG, "Skip starting FileDaemonService: app is in background");
                return false;
            }

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
