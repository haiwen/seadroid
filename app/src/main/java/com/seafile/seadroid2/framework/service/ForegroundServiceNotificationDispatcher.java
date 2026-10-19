package com.seafile.seadroid2.framework.service;

import android.content.Context;

import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.notification.NotificationInfo;

/**
 * Retained source compatibility; no independently owned transfer foreground notifications.
 */
@Deprecated
public class ForegroundServiceNotificationDispatcher extends BackupThreadNotificationDispatcher {
    public ForegroundServiceNotificationDispatcher(Context context) {
        super(context);
    }

    public NotificationInfo getForegroundNotification(FeatureDataSource source) {
        return null;
    }

    public NotificationInfo getForegroundNotification(FeatureDataSource source, String subtitle) {
        return null;
    }

    public void releaseAcquire(FeatureDataSource source) {
    }
}
