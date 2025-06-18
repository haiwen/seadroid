package com.seafile.seadroid2.framework.notification;

import android.app.Notification;

public class NotificationInfo {
    public final int id;
    public final Notification notification;

    public NotificationInfo(int id, Notification notification) {
        if (notification == null) {
            throw new IllegalArgumentException("Foreground notification must not be null");
        }
        this.id = id;
        this.notification = notification;
    }
}
