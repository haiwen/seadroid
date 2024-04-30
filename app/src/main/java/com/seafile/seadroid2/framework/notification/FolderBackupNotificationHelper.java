package com.seafile.seadroid2.framework.notification;

import static com.seafile.seadroid2.framework.notification.base.NotificationUtils.NOTIFICATION_MESSAGE_KEY;
import static com.seafile.seadroid2.framework.notification.base.NotificationUtils.NOTIFICATION_OPEN_UPLOAD_TAB;

import android.content.Context;
import android.content.Intent;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.ui.transfer_list.TransferActivity;

public class FolderBackupNotificationHelper extends BaseTransferNotificationHelper {
    public FolderBackupNotificationHelper(Context context) {
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
        String uploading = context.getString(R.string.uploading);
        String title = context.getString(R.string.settings_folder_backup_info_title);
        return title + " " + uploading;
    }

    @Override
    public int getMaxProgress() {
        return 100;
    }

    @Override
    public String getChannelId() {
        return NotificationUtils.NOTIFICATION_CHANNEL_UPLOAD_FOLDER;
    }

    @Override
    public int getNotificationId() {
        return NotificationUtils.NOTIFICATION_UPLOAD_FOLDER_ID;
    }
}
