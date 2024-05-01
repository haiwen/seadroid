package com.seafile.seadroid2.framework.notification.base;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

public class NotificationUtils {
    public static final String NOTIFICATION_CHANNEL_GENERAL = "NOTIFICATION_CHANNEL_GENERAL";
    public static final String NOTIFICATION_CHANNEL_DOWNLOAD = "NOTIFICATION_CHANNEL_DOWNLOAD";
    public static final String NOTIFICATION_CHANNEL_UPLOAD = "NOTIFICATION_CHANNEL_UPLOAD";
    public static final String NOTIFICATION_CHANNEL_UPLOAD_FOLDER = "NOTIFICATION_CHANNEL_UPLOAD_FOLDER";

    public static final String NOTIFICATION_CHANNEL_UPLOAD_FILE = "NOTIFICATION_CHANNEL_UPLOAD_FILE";
    public static final String NOTIFICATION_CHANNEL_UPLOAD_ALBUM_BACKUP = "NOTIFICATION_CHANNEL_ALBUM_BACKUP";


    public static final int NOTIFICATION_ERROR_ID = 100;
    public static final int NOTIFICATION_GENERAL_ID = 1010;
    public static final int NOTIFICATION_DOWNLOAD_ID = 2000;
    public static final int NOTIFICATION_UPLOAD_ID = 3000;
    public static final int NOTIFICATION_UPLOAD_FOLDER_ID = 3010;
    public static final int NOTIFICATION_UPLOAD_FILE_ID = 3011;
    public static final int NOTIFICATION_UPLOAD_ALBUM_BACKUP_ID = 3020;


    public static final String NOTIFICATION_MESSAGE_KEY = "notification_key";
    public static final String NOTIFICATION_OPEN_UPLOAD_TAB = "open upload tab notification";
    public static final String NOTIFICATION_OPEN_DOWNLOAD_TAB = "open download tab notification";

    private NotificationUtils() {
    }

    public static void initNotificationChannels(Context context) {
        if (context == null) {
            return;
        }

        NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        if (notificationManager == null) {
            SLogs.d("NotificationManager is null.");
            return;
        }

        createChannel(context, notificationManager,
                NOTIFICATION_CHANNEL_DOWNLOAD,
                R.string.channel_name_download,
                R.string.notification_download_started_title, NotificationManager.IMPORTANCE_DEFAULT);

        createChannel(context, notificationManager,
                NOTIFICATION_CHANNEL_UPLOAD,
                R.string.channel_name_upload,
                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);

        createChannel(context, notificationManager,
                NOTIFICATION_CHANNEL_UPLOAD_FOLDER,
                R.string.channel_name_upload,
                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);

        createChannel(context, notificationManager,
                NOTIFICATION_CHANNEL_UPLOAD_FILE,
                R.string.channel_name_upload,
                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);

        createChannel(context, notificationManager,
                NOTIFICATION_CHANNEL_UPLOAD_ALBUM_BACKUP,
                R.string.channel_name_album_backup,
                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);


        createChannel(context, notificationManager,
                NotificationUtils.NOTIFICATION_CHANNEL_GENERAL,
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
