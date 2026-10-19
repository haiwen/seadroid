package com.seafile.seadroid2.framework.notification;

import android.app.Notification;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;

import androidx.core.app.NotificationCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.ui.main.MainActivity;

/**
 * Builds only; the Service exclusively owns foreground start/stop and notification removal.
 */
public final class PersistentNotificationFactory {
    private PersistentNotificationFactory() {
    }

    public static Notification fileMonitor(Context context) {
        NotificationUtils.initNotificationChannels(context);
        Intent intent = new Intent(context, MainActivity.class).setAction(context.getPackageName() + ".fileMonitor");
        PendingIntent click = PendingIntent.getActivity(context, NotificationUtils.NID_FILE_MONITOR_PERSISTENTLY,
                intent, PendingIntent.FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT);
        return new NotificationCompat.Builder(context, NotificationUtils.FILE_MONITOR_CHANNEL)
                .setSmallIcon(R.drawable.icon)
                .setContentTitle(context.getString(R.string.notification_background_file_monitor_title))
                .setContentText(context.getString(R.string.notification_background_file_monitor_content))
                .setOngoing(true).setAutoCancel(false).setOnlyAlertOnce(true)
                .setPriority(NotificationCompat.PRIORITY_LOW).setCategory(Notification.CATEGORY_SERVICE)
                .setVisibility(NotificationCompat.VISIBILITY_PUBLIC).setContentIntent(click).build();
    }
}
