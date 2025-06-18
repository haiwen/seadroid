package com.seafile.seadroid2.framework.notification.base;

import static android.app.PendingIntent.FLAG_IMMUTABLE;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;

import androidx.activity.contextaware.ContextAware;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.core.app.NotificationCompat;
import androidx.core.content.ContextCompat;
import androidx.work.ForegroundInfo;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public abstract class BaseNotification {
    private final int REQ_CODE = 1;
    private final ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
    private NotificationManager notificationManager;
    protected Context context;
    private long last_time = 0;

    public abstract String getChannelId();

    public abstract int getMaxProgress();

    private boolean hasPermission = true;

    public BaseNotification(Context context) {
        this.context = context;
    }

    public NotificationManager getNotificationManager() {
        if (notificationManager == null) {
            notificationManager = context.getSystemService(NotificationManager.class);
        }
        return notificationManager;
    }

    @Nullable
    public NotificationCompat.Builder getNotificationBuilder(String title, String content, Intent intent) {
        if (!hasPermission) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                int i = ContextCompat.checkSelfPermission(context, android.Manifest.permission.POST_NOTIFICATIONS);
                hasPermission = i == android.content.pm.PackageManager.PERMISSION_GRANTED;
            }
        }

        if (!hasPermission) {
            return null;
        }

        NotificationCompat.Builder builder = new NotificationCompat.Builder(context, getChannelId());

        builder.setSmallIcon(R.drawable.icon);
        builder.setContentTitle(title);
        builder.setContentText(content);
        builder.setPriority(NotificationCompat.PRIORITY_HIGH);

        if (getMaxProgress() > 0) {
            builder.setOnlyAlertOnce(true);
            builder.setOngoing(true);
            builder.setProgress(getMaxProgress(), 0, false);
            builder.setCategory(NotificationCompat.CATEGORY_PROGRESS);
        } else {
            builder.setOnlyAlertOnce(true);
            builder.setOngoing(false);
            builder.setAutoCancel(true);
            builder.setCategory(NotificationCompat.CATEGORY_RECOMMENDATION);
            builder.setSilent(false);
        }

        if (null == intent) {
            builder.setContentIntent(null);
        } else {
            PendingIntent pendingIntent = PendingIntent.getActivity(context,
                    REQ_CODE,
                    intent,
                    FLAG_IMMUTABLE);
            builder.setContentIntent(pendingIntent);
        }
        return builder;
    }

    @Nullable
    public Notification getNotification(String title, String content, Intent intent) {
        NotificationCompat.Builder builder = getNotificationBuilder(title, content, intent);
        if (builder == null) {
            return null;
        }
        return builder.build();
    }

    public void showNotification(int nid, String title) {
        showNotification(nid, title, null, null);
    }

    public void showNotification(int nid, String title, String content) {
        showNotification(nid, title, content, null);
    }

    public void showNotification(int nid, String title, String content, Intent intent) {
        Notification notification = getNotification(title, content, intent);
        if (notification != null) {
            getNotificationManager().notify(nid, notification);
        } else {
            SLogs.e("Notification is null");
        }
    }

    public void notifyProgress(int nid, String title, String subTitle, int percent, Intent intent) {
        if (!hasPermission) {
            return;
        }

        long now = System.currentTimeMillis();
        if (now - last_time < 1000) {
            return;
        }
        last_time = now;

        NotificationCompat.Builder builder = getNotificationBuilder(title, subTitle, intent);
        if (builder == null) {
            return;
        }

        builder.setProgress(getMaxProgress(), percent, false);

        String progressStr = context.getString(R.string.notification_upload_upload_in_progress);
        String text = String.format(progressStr, percent, subTitle);

        showNotification(nid, title, text, intent);
    }

    public ForegroundInfo getForegroundProgressNotification(int nid, String title, String subTitle, int percent, int totalCount, Intent intent) {
        if (!hasPermission) {
            return null;
        }

        long now = System.currentTimeMillis();
        if (now - last_time < 1000) {
            return null;
        }
        last_time = now;

        NotificationCompat.Builder builder = getNotificationBuilder(title, subTitle, intent);
        if (builder == null) {
            return null;
        }
        builder.setProgress(getMaxProgress(), percent, false);

        if (totalCount > 0) {
            subTitle = subTitle + " / " + totalCount;
        }

        String progressStr = context.getString(R.string.notification_upload_upload_in_progress);
        String text = String.format(progressStr, percent, subTitle);

        return getForegroundNotification(nid, title, text, intent);
    }

    public ForegroundInfo getForegroundNotification(int nid, String title, String content, Intent intent) {
        if (!hasPermission) {
            return null;
        }
        NotificationCompat.Builder builder = getNotificationBuilder(title, content, intent);
        if (builder == null) {
            return null;
        }
        builder.setContentTitle(title);
        builder.setContentText(content);

        if (null == intent) {
            builder.setContentIntent(null);
        } else {
            PendingIntent pendingIntent = PendingIntent.getActivity(context,
                    REQ_CODE,
                    intent,
                    FLAG_IMMUTABLE);
            builder.setContentIntent(pendingIntent);
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            return new ForegroundInfo(nid, builder.build(), ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            return new ForegroundInfo(nid, builder.build());
        }
    }

    /// cancel
    public void cancel(int nid) {
        cancel(nid, 0);
    }

    /**
     * Delay for a while before cancel notification in order user can see the result
     */
    public void cancel(int nid, long delayInMillis) {
        if (delayInMillis <= 0) {
            getNotificationManager().cancel(nid);
        } else {
            executorService.schedule(() -> {
                try {
                    getNotificationManager().cancel(nid);
                } catch (Exception e) {
                    System.err.println("Failed to cancel notification with ID " + nid + ": " + e.getMessage());
                }
            }, delayInMillis, TimeUnit.MILLISECONDS);
        }
    }

}
