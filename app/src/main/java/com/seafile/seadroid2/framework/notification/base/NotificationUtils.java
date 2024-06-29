package com.seafile.seadroid2.framework.notification.base;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

public class NotificationUtils {
    public static final String NOTIFICATION_CHANNEL_GENERAL = "NOTIFICATION_CHANNEL_GENERAL";
    public static final String NOTIFICATION_CHANNEL_TRANSFER = "NOTIFICATION_CHANNEL_TRANSFER";
//    public static final String NOTIFICATION_CHANNEL_UPLOAD = "NOTIFICATION_CHANNEL_UPLOAD";
//    public static final String NOTIFICATION_CHANNEL_UPLOAD_FOLDER = "NOTIFICATION_CHANNEL_UPLOAD_FOLDER";
//    public static final String NOTIFICATION_CHANNEL_UPLOAD_FILE = "NOTIFICATION_CHANNEL_UPLOAD_FILE";
//    public static final String NOTIFICATION_CHANNEL_UPLOAD_ALBUM_BACKUP = "NOTIFICATION_CHANNEL_ALBUM_BACKUP";


    public static final String NOTIFICATION_CHANNEL_OPEN_APK = "NOTIFICATION_CHANNEL_OPEN_APK_FILE";


    public static final int NOTIFICATION_ID_ERROR = 100;
    public static final int NOTIFICATION_ID_GENERAL = 1010;
    public static final int NOTIFICATION_ID_DOWNLOAD = 2000;

    //folder
    public static final int NOTIFICATION_ID_UPLOAD_FOLDER_SCAN = 3010;
    public static final int NOTIFICATION_ID_UPLOAD_FOLDER = 3011;
    public static final int NOTIFICATION_ID_UPLOAD_FILE = 3011;

    //album
    public static final int NOTIFICATION_ID_UPLOAD_ALBUM_BACKUP = 3020;
    public static final int NOTIFICATION_ID_UPLOAD_ALBUM_BACKUP_SCAN = 3021;


    public static final int NOTIFICATION_ID_OPEN_APK = 4000;


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
            SLogs.d("NotificationManager is null.");
            return;
        }

        createChannel(context, notificationManager,
                NOTIFICATION_CHANNEL_TRANSFER,
                R.string.channel_name_transfer,
                R.string.channel_name_transfer, NotificationManager.IMPORTANCE_DEFAULT);


//        createChannel(context, notificationManager,
//                NOTIFICATION_CHANNEL_TRANSFER,
//                R.string.channel_name_download,
//                R.string.notification_download_started_title, NotificationManager.IMPORTANCE_DEFAULT);
//
//        createChannel(context, notificationManager,
//                NOTIFICATION_CHANNEL_TRANSFER,
//                R.string.channel_name_upload,
//                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);
//
//        createChannel(context, notificationManager,
//                NOTIFICATION_CHANNEL_TRANSFER,
//                R.string.channel_name_upload,
//                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);
//
//        createChannel(context, notificationManager,
//                NOTIFICATION_CHANNEL_TRANSFER,
//                R.string.channel_name_upload,
//                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);
//
//        createChannel(context, notificationManager,
//                NOTIFICATION_CHANNEL_TRANSFER,
//                R.string.channel_name_album_backup,
//                R.string.notification_upload_started_title, NotificationManager.IMPORTANCE_DEFAULT);

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
