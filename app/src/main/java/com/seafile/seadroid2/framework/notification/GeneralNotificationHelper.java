package com.seafile.seadroid2.framework.notification;

import android.content.Context;
import android.content.Intent;

import androidx.annotation.StringRes;

import com.seafile.seadroid2.framework.notification.base.BaseNotification;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;

public class GeneralNotificationHelper extends BaseNotification {
    @Override
    public String getChannelId() {
        return NotificationUtils.GENERAL_CHANNEL;
    }

    @Override
    public int getMaxProgress() {
        return 0;
    }

    public GeneralNotificationHelper(Context context) {
        super(context);
    }

    ////////////////
    /// general
    ////////////////
    public void showNotification(@StringRes int titleRes) {
        super.showNotification(NotificationUtils.NID_GENERAL, context.getString(titleRes), null, null);
    }

    public void showNotification(String title) {
        super.showNotification(NotificationUtils.NID_GENERAL, title, null, null);
    }

    public void showNotification(@StringRes int titleRes, @StringRes int contentRes) {
        super.showNotification(NotificationUtils.NID_GENERAL, context.getString(titleRes), context.getString(contentRes), null);
    }

    public void showNotification(String title, String content) {
        super.showNotification(NotificationUtils.NID_GENERAL, title, content, null);
    }

    public void showNotification(@StringRes int titleRes, @StringRes int contentRes, Intent intent) {
        super.showNotification(NotificationUtils.NID_GENERAL, context.getString(titleRes), context.getString(contentRes), intent);
    }

    ////////////////
    /// error
    ////////////////
    public void showErrorNotification(@StringRes int titleRes) {
        super.showNotification(NotificationUtils.NID_ERROR, context.getString(titleRes), null, null);
    }

    public void showErrorNotification(String title) {
        super.showNotification(NotificationUtils.NID_ERROR, title, null, null);
    }

    public void showErrorNotification(@StringRes int titleRes, @StringRes int contentRes) {
        super.showNotification(NotificationUtils.NID_ERROR, context.getString(titleRes), context.getString(contentRes), null);
    }

    public void showErrorNotification(String title, String content) {
        super.showNotification(NotificationUtils.NID_ERROR, title, content, null);
    }

    public void showErrorNotification(@StringRes int titleRes, String content) {
        super.showNotification(NotificationUtils.NID_ERROR, context.getString(titleRes), content, null);
    }

    public void showErrorNotification(String title, @StringRes int contentRes) {
        super.showNotification(NotificationUtils.NID_ERROR, title, context.getString(contentRes), null);
    }

    public void showErrorNotification(@StringRes int titleRes, @StringRes int contentRes, Intent intent) {
        super.showNotification(NotificationUtils.NID_ERROR, context.getString(titleRes), context.getString(contentRes), intent);
    }
}
