package com.seafile.seadroid2.framework.notification.base;

import android.content.Context;
import androidx.work.ForegroundInfo;

/**
 * Obsolete stub for Worker compatibility.
 */
@Deprecated
public class BaseTransferNotificationHelper extends BaseNotification {
    public BaseTransferNotificationHelper(Context context) {
        super(context);
    }

    @Override
    public String getChannelId() {
        return "";
    }

    @Override
    public int getMaxProgress() {
        return 0;
    }

    public ForegroundInfo getForegroundNotification() {
        return null;
    }

    public ForegroundInfo getForegroundNotification(int resId) {
        return null;
    }

    public ForegroundInfo getForegroundNotification(String title, String subTitle) {
        return null;
    }

    public ForegroundInfo getForegroundProgressNotification(String title, int percent) {
        return null;
    }

    public void cancel() {
    }
}
