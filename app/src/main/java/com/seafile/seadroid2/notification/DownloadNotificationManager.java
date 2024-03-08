package com.seafile.seadroid2.notification;

import static com.seafile.seadroid2.notification.NotificationUtils.NOTIFICATION_MESSAGE_KEY;
import static com.seafile.seadroid2.notification.NotificationUtils.NOTIFICATION_OPEN_DOWNLOAD_TAB;

import android.content.Context;
import android.content.Intent;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.transfer.TransferActivity;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.StringUtils;

import org.checkerframework.checker.units.qual.N;

import java.util.Random;
import java.util.UUID;

public class DownloadNotificationManager extends BaseTransferNotificationManager {

    public DownloadNotificationManager(Context context) {
        super(context);

    }

    @Override
    public Intent getTransferIntent() {
        Intent dIntent = new Intent(context, TransferActivity.class);
        dIntent.putExtra(NOTIFICATION_MESSAGE_KEY, NOTIFICATION_OPEN_DOWNLOAD_TAB);
        dIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        return dIntent;
    }

    @Override
    public String getNotificationTitle() {
        return context.getString(R.string.notification_download_started_title);
    }

    @Override
    public String getNotificationChannelId() {
        return NotificationUtils.NOTIFICATION_CHANNEL_DOWNLOAD;
    }

    @Override
    public int getNotificationId() {
        return NotificationUtils.NOTIFICATION_DOWNLOAD_ID;
    }
}
