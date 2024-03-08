package com.seafile.seadroid2.notification;

import android.content.Context;
import android.content.Intent;

import com.seafile.seadroid2.R;

public class AlbumBackupNotificationManager extends BaseTransferNotificationManager {
    public AlbumBackupNotificationManager(Context context) {
        super(context);
    }

    @Override
    public Intent getTransferIntent() {
        return null;
    }

    @Override
    public String getNotificationTitle() {
        return context.getString(R.string.notification_upload_started_title);
    }

    @Override
    public String getNotificationChannelId() {
        return NotificationUtils.NOTIFICATION_CHANNEL_ALBUM_BACKUP;
    }

    @Override
    public int getNotificationId() {
        return NotificationUtils.NOTIFICATION_ALBUM_BACKUP_ID;
    }
}
