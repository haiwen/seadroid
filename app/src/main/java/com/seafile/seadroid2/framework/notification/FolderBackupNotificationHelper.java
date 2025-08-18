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
        return dIntent;
    }

    @Override
    public String getDefaultTitle() {
        return context.getString(R.string.settings_folder_backup_info_title);
    }

    @Override
    public String getDefaultSubtitle() {
        return context.getString(R.string.uploading);
    }

    @Override
    public int getMaxProgress() {
        return 100;
    }

    @Override
    public String getChannelId() {
        return NotificationUtils.FILE_TRANSFER_CHANNEL;
    }

    @Override
    public int getNotificationId() {
        return NotificationUtils.NID_FOLDER_BACKUP;
    }
}
