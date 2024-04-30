package com.seafile.seadroid2.framework.notification.base;

import android.content.Context;
import android.content.Intent;

import androidx.annotation.StringRes;

import com.seafile.seadroid2.R;

public abstract class BaseTransferNotificationHelper extends BaseNotification {

    public abstract Intent getTransferIntent();

    public abstract String getNotificationTitle();

    public abstract String getChannelId();

    public abstract int getNotificationId();

    protected BaseTransferNotificationHelper(Context context) {
        super(context);
    }

    public void showNotification() {
        super.showNotification(getNotificationId(), getNotificationTitle(), null, getTransferIntent());
    }

    public void showNotification(String title) {
        super.showNotification(getNotificationId(), title, context.getString(R.string.wait), getTransferIntent());
    }

    public void showNotification(@StringRes int titleRes) {
        String title = context.getString(titleRes);
        super.showNotification(getNotificationId(), title, context.getString(R.string.wait), getTransferIntent());
    }

    public void showNotification(@StringRes int titleRes, @StringRes int contentRes) {
        super.showNotification(getNotificationId(), context.getString(titleRes), context.getString(contentRes), getTransferIntent());
    }

    public void notifyProgress(String fileName, int percent) {
        super.notifyProgress(getNotificationId(), fileName, getNotificationTitle(), percent, getTransferIntent());
    }

    public void cancel() {
        super.cancel(getNotificationId(), 0);
    }

    public void cancel(int delayInMillis) {
        super.cancel(getNotificationId(), delayInMillis);
    }
}
