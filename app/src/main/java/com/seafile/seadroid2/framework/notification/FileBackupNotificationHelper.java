package com.seafile.seadroid2.framework.notification;

import android.content.Context;
import android.content.Intent;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;

public class FileBackupNotificationHelper extends BaseTransferNotificationHelper {
    public FileBackupNotificationHelper(Context context) {
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
    public int getMaxProgress() {
        return 100;
    }

    @Override
    public String getChannelId() {
        return NotificationUtils.NOTIFICATION_CHANNEL_UPLOAD_FILE;
    }

    @Override
    public int getNotificationId() {
        return NotificationUtils.NOTIFICATION_UPLOAD_FILE_ID;
    }
}
