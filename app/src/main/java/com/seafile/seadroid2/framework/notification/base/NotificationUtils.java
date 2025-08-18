package com.seafile.seadroid2.framework.notification.base;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

public class NotificationUtils {
    public static final String GENERAL_CHANNEL = "NOTIFICATION_CHANNEL_GENERAL";
    public static final String FILE_TRANSFER_CHANNEL = "NOTIFICATION_CHANNEL_FILE_TRANSFER";
    public static final String OPEN_APK_CHANNEL = "NOTIFICATION_CHANNEL_OPEN_APK_FILE";

    public static final int NID_GENERAL = 1000;
    public static final int NID_ERROR = 1100;
    public static final int NID_TRANSFER = 3000;
    public static final int NID_OPEN_APK = 4000;

    //album
    public static final int NID_ALBUM_BACKUP = 3100;
    public static final int NID_ALBUM_BACKUP_SCAN = 3101;

    //folder
    public static final int NID_FOLDER_BACKUP_SCAN = 3200;
    public static final int NID_FOLDER_BACKUP = 3201;

    //file
    public static final int NID_FILE_UPLOAD = 3300;
    public static final int NID_LOCAL_FILE_UPDATE = 3301;

    //download
    public static final int NID_FILE_DOWNLOAD = 3400;


    public static final String NOTIFICATION_MESSAGE_KEY = "notification_key";
    public static final String NOTIFICATION_OPEN_UPLOAD_TAB = "open_upload_tab_notification";
    public static final String NOTIFICATION_OPEN_DOWNLOAD_TAB = "open_download_tab_notification";

    private NotificationUtils() {
    }

    public static void initNotificationChannels(Context context) {
        if (context == null) {
            return;
        }

        NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        if (notificationManager == null) {
            SLogs.d("NotificationUtils", "NotificationManager is null.");
            return;
        }

        createChannel(context, notificationManager,
                FILE_TRANSFER_CHANNEL,
                R.string.channel_name_transfer,
                R.string.channel_name_transfer, NotificationManager.IMPORTANCE_HIGH);

        createChannel(context, notificationManager,
                GENERAL_CHANNEL,
                R.string.app_name,
                R.string.app_name, NotificationManager.IMPORTANCE_HIGH);
    }


    private static void createChannel(Context context, NotificationManager notificationManager,
                                      String channelId, int channelName,
                                      int channelDescription, int importance) {
        createChannel(notificationManager,
                channelId,
                channelName,
                channelDescription,
                context,
                importance, false, false);
    }

    private static void createChannel(NotificationManager notificationManager,
                                      String channelId, int channelName,
                                      int channelDescription, Context context,
                                      int importance, boolean isVibrate, boolean hasSound) {
        if (context == null) {
            return;
        }

        CharSequence name = context.getString(channelName);
        String description = context.getString(channelDescription);
        NotificationChannel channel = new NotificationChannel(channelId, name, importance);
        channel.setShowBadge(false);
        channel.setDescription(description);
        channel.enableLights(false);
        channel.enableVibration(isVibrate);
        if (!hasSound) {
            channel.setSound(null, null);
        }
        notificationManager.createNotificationChannel(channel);
    }
}
