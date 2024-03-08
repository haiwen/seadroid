package com.seafile.seadroid2.notification;

import static com.seafile.seadroid2.notification.NotificationUtils.NOTIFICATION_MESSAGE_KEY;
import static com.seafile.seadroid2.notification.NotificationUtils.NOTIFICATION_OPEN_UPLOAD_TAB;

import android.content.Context;
import android.content.Intent;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.transfer.TransferActivity;

public class UploadNotificationManager extends BaseTransferNotificationManager {
    public UploadNotificationManager(Context context) {
        super(context);
    }

    @Override
    public Intent getTransferIntent() {
        Intent dIntent = new Intent(context, TransferActivity.class);
        dIntent.putExtra(NOTIFICATION_MESSAGE_KEY, NOTIFICATION_OPEN_UPLOAD_TAB);
        dIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        return dIntent;
    }

    @Override
    public String getNotificationTitle() {
        return context.getString(R.string.notification_upload_started_title);
    }

    @Override
    public String getNotificationChannelId() {
        return NotificationUtils.NOTIFICATION_CHANNEL_UPLOAD;
    }

    @Override
    public int getNotificationId() {
        return NotificationUtils.NOTIFICATION_UPLOAD_ID;
    }
}
