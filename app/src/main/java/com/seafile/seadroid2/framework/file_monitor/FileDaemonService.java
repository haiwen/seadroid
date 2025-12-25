package com.seafile.seadroid2.framework.file_monitor;

import static android.app.PendingIntent.FLAG_IMMUTABLE;

import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.main.MainActivity;

public class FileDaemonService extends Service {
    private final String TAG = "FileDaemonService";

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        SLogs.e(TAG, "onCreate()");

    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        SLogs.e(TAG, "onStartCommand()", "file daemon service started");

        // Android 14 requires specifying a type, assuming you define dataSync in the manifest
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.UPSIDE_DOWN_CAKE) {
            startForeground(
                    NotificationUtils.NID_FILE_MONITOR_PERSISTENTLY,
                    buildNotification(),
                    ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC
            );
        } else {
            startForeground(NotificationUtils.NID_FILE_MONITOR_PERSISTENTLY, buildNotification());
        }

        startPeriodicScanTask();
        return START_STICKY;
    }


    private Notification buildNotification() {

        Intent intent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 1, intent, FLAG_IMMUTABLE);

        return new NotificationCompat.Builder(this, NotificationUtils.FILE_MONITOR_CHANNEL)
                .setSmallIcon(R.drawable.icon)
                .setContentTitle(getString(R.string.notification_background_file_monitor_title))
                .setContentText(getString(R.string.notification_background_file_monitor_content))
                .setOngoing(true)
                .setAutoCancel(false)
                .setPriority(NotificationCompat.PRIORITY_MAX)
                .setCategory(Notification.CATEGORY_SERVICE)
                .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
                .setContentIntent(pendingIntent)
                .build();
    }

    private final Handler periodicHandler = new Handler(Looper.getMainLooper());
    private boolean isPeriodicRunning = false;
    private final long periodicTime = 5 * 60 * 1000L;

    private final Runnable periodicTask = new Runnable() {
        @Override
        public void run() {
            if (isPeriodicRunning) {
                SLogs.d(TAG, "Periodic scan skipped (previous still running)");
                periodicHandler.postDelayed(this, periodicTime);
                return;
            }

            isPeriodicRunning = true;

            try {
                SLogs.d(TAG, "Periodic local file scan...");
                startWorkers();
            } catch (Exception e) {
                SLogs.e(TAG, "Periodic scan failed", e);
            } finally {
                SLogs.e(TAG, "post a delay task", "delay time: " + periodicTime);

                isPeriodicRunning = false;
                periodicHandler.postDelayed(this, periodicTime);
            }
        }
    };

    private void startPeriodicScanTask() {
        periodicHandler.removeCallbacks(periodicTask);
        SLogs.e(TAG, "post a delay task", "delay time: " + periodicTime);

        periodicHandler.postDelayed(periodicTask, periodicTime);
    }

    private void startWorkers() {
        if (AlbumBackupSharePreferenceHelper.isAlbumBackupEnable()) {
            CameraUploadManager.getInstance().performSync();
        }

        if (FolderBackupSharePreferenceHelper.isFolderBackupEnable()) {
            BackupThreadExecutor.getInstance().runFolderBackupFuture(true);
        }
    }

    @Override
    public void onDestroy() {
        SLogs.e(TAG, "onDestroy()", "file daemon service destroy");

        // 1. Stop all scheduled tasks immediately to prevent new workers from starting during the shutdown
        periodicHandler.removeCallbacks(periodicTask);
        isPeriodicRunning = false;

        // 2. Remove foreground notifications (must be completed before service destruction is complete)
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            stopForeground(STOP_FOREGROUND_REMOVE);
        } else {
            stopForeground(true);
        }

        super.onDestroy();
    }

    public void stopDaemon() {
        stopSelf();
    }

}
