package com.seafile.seadroid2.framework.notification;

import android.content.Context;
import android.content.Intent;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;

public class AlbumBackupScanNotificationHelper extends BaseTransferNotificationHelper {
    public AlbumBackupScanNotificationHelper(Context context) {
        super(context);
    }

    @Override
    public Intent getTransferIntent() {
        return null;
    }

    @Override
    public String getDefaultTitle() {
        return context.getString(R.string.settings_camera_upload_info_title);
    }

    @Override
    public String getDefaultSubtitle() {
        return context.getString(R.string.is_scanning);
    }

    @Override
    public int getMaxProgress() {
        return 0;
    }

    @Override
    public String getChannelId() {
        return NotificationUtils.NOTIFICATION_CHANNEL_TRANSFER;
    }

    @Override
    public int getNotificationId() {
        return NotificationUtils.NID_UPLOAD_ALBUM_BACKUP_SCAN;
    }
}
