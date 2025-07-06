package com.seafile.seadroid2.ui.camera_upload;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.text.TextUtils;

import androidx.core.content.ContextCompat;

import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SLogs;

public class AlbumBackupAdapterBridge {
    private static final String TAG = "AlbumBackupAdapterBridge";
    public static final String ACTION_SYNC_TASK = "ACTION_SYNC_ALBUM_TASK";

    public static void syncAlbumBackup(Context context) {
        syncAlbumBackup(context, false);
    }

    public static void syncAlbumBackup(Context context, boolean isFullScan) {
        Intent intent = new Intent(ACTION_SYNC_TASK);
        intent.setPackage(context.getPackageName());
        intent.putExtra("isFullScan", isFullScan);
        context.sendBroadcast(intent);
    }

    public static void registerSyncReceiver(Context context) {
        if (context == null) {
            return;
        }

        IntentFilter filter = new IntentFilter(ACTION_SYNC_TASK);
        ContextCompat.registerReceiver(context, receiver, filter, ContextCompat.RECEIVER_NOT_EXPORTED);
    }

    private final static BroadcastReceiver receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            if (intent == null) {
                SLogs.d(TAG, "onReceive()", "intent is null");
                return;
            }

            SLogs.d(TAG, "onReceive()", "action: " + intent.getAction());

            if (TextUtils.equals(ACTION_SYNC_TASK, intent.getAction())) {
                boolean isFullScan = intent.getBooleanExtra("isFullScan", false);
                BackupThreadExecutor.getInstance().runAlbumBackupTask(isFullScan);
            }
        }
    };
}
