package com.seafile.seadroid2.notification;

import static android.app.PendingIntent.FLAG_IMMUTABLE;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;

import androidx.core.app.NotificationCompat;

import com.seafile.seadroid2.R;

public abstract class BaseTransferNotificationManager {

    private final int REQ_CODE = 1;

    protected Context context;

    private NotificationCompat.Builder builder;
    private NotificationManager notificationManager;
    private Notification notification;

    private int lastPercent = 0;

    protected BaseTransferNotificationManager(Context context) {
        this.context = context;

        notificationManager = context.getSystemService(NotificationManager.class);

        prepare();
    }

    private void prepare() {
        builder = new NotificationCompat.Builder(context, getNotificationChannelId())
                .setSmallIcon(R.drawable.icon)
                .setOnlyAlertOnce(true)
                .setContentTitle(getNotificationTitle())
                .setOngoing(true)
                .setProgress(100, 0, false);

        Intent intent = getTransferIntent();
        if (intent != null) {
            PendingIntent pendingIntent = PendingIntent.getActivity(context,
                    REQ_CODE,
                    getTransferIntent(),
                    FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT);
            builder.setContentIntent(pendingIntent);
        }

        notification = builder.build();
    }

    public Notification getNotification() {
        return notification;
    }

    public void showNotification() {
        notificationManager.notify(getNotificationId(), builder.build());
    }

    public void dismissNotification() {
        notificationManager.cancel(getNotificationId());
    }

    public void waitForShowNotification(String fileName) {
        builder.setProgress(100, 0, false);

        String waitingStr = context.getString(R.string.download_waiting);

        builder.setContentTitle(fileName);
        builder.setContentText(waitingStr);

        showNotification();
    }

    public void updateProgress(String fileName, int percent) {
        if (percent == lastPercent) {
            return;
        }

        lastPercent = percent;

        builder.setProgress(100, percent, false);

        String titleStr = getNotificationTitle();
        String progressStr = context.getString(R.string.notification_upload_upload_in_progress);

        String text = String.format(progressStr, percent, titleStr);
        builder.setContentTitle(fileName);
        builder.setContentText(text);

        showNotification();
    }

    public abstract Intent getTransferIntent();

    public abstract String getNotificationTitle();

    public abstract String getNotificationChannelId();

    public abstract int getNotificationId();

}
