package com.seafile.seadroid2.framework.notification.base;

import android.app.Notification;
import android.content.Context;
import android.content.Intent;

import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.work.ForegroundInfo;

import com.seafile.seadroid2.R;

public abstract class BaseTransferNotificationHelper extends BaseNotification {

    public abstract Intent getTransferIntent();

    public abstract String getDefaultTitle();

    public abstract String getDefaultSubtitle();

    public abstract String getChannelId();

    public abstract int getNotificationId();

    protected BaseTransferNotificationHelper(Context context) {
        super(context);
    }

//
//    public void showNotification() {
//        super.showNotification(getNotificationId(), getDefaultTitle(), getDefaultSubtitle(), getTransferIntent());
//    }
//
//    public void showNotification(String title) {
//        super.showNotification(getNotificationId(), title, getDefaultSubtitle(), getTransferIntent());
//    }
//
//    public void showNotification(@StringRes int titleRes) {
//        super.showNotification(getNotificationId(), context.getString(titleRes), getDefaultSubtitle(), getTransferIntent());
//    }
//
//    public void showNotification(@StringRes int titleRes, @StringRes int contentRes) {
//        super.showNotification(getNotificationId(), context.getString(titleRes), context.getString(contentRes), getTransferIntent());
//    }

    public void notifyProgress(String fileName, int percent) {
        super.notifyProgress(getNotificationId(), fileName, getDefaultTitle(), percent, getTransferIntent());
    }

    public void showDefaultNotification() {
        super.showNotification(getNotificationId(), getDefaultTitle(), getDefaultSubtitle(), getTransferIntent());
    }

    @Nullable
    public Notification getNotification() {
        return super.getNotification(getDefaultTitle(), getDefaultSubtitle(), getTransferIntent());
    }

    //Foreground Notification
    public ForegroundInfo getForegroundNotification() {
        return super.getForegroundNotification(getNotificationId(), getDefaultTitle(), getDefaultSubtitle(), getTransferIntent());
    }

    public ForegroundInfo getForegroundNotification(String title) {
        return super.getForegroundNotification(getNotificationId(), title, getDefaultSubtitle(), getTransferIntent());
    }

    public ForegroundInfo getForegroundNotification(String title, String content) {
        return super.getForegroundNotification(getNotificationId(), title, content, getTransferIntent());
    }

    public ForegroundInfo getForegroundNotification(@StringRes int titleRes) {
        return super.getForegroundNotification(getNotificationId(), context.getString(titleRes), getDefaultSubtitle(), getTransferIntent());
    }

    public ForegroundInfo getForegroundNotification(@StringRes int titleRes, @StringRes int contentRes) {
        return super.getForegroundNotification(getNotificationId(), context.getString(titleRes), context.getString(contentRes), getTransferIntent());
    }

    //Progress Foreground Notification
    public ForegroundInfo getForegroundProgressNotification(String fileName, int percent) {
        return super.getForegroundProgressNotification(getNotificationId(), fileName, getDefaultSubtitle(), percent, 0, getTransferIntent());
    }

    public ForegroundInfo getForegroundProgressNotification(String fileName, int percent, int totalCount) {
        return super.getForegroundProgressNotification(getNotificationId(), fileName, getDefaultSubtitle(), percent, totalCount, getTransferIntent());
    }

    public void cancel() {
        super.cancel(getNotificationId(), 0);
    }

    public void cancel(int delayInMillis) {
        super.cancel(getNotificationId(), delayInMillis);
    }
}
