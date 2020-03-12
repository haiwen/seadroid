package com.seafile.seadroid2.ui;

import android.app.Notification;
import android.content.Context;
import android.os.Build;
import android.support.v4.app.NotificationCompat;
import android.view.View;
import android.widget.RemoteViews;
import com.seafile.seadroid2.R;

/**
 * Extends the support class {@link android.support.v4.app.NotificationCompat.Builder} to grant that
 * a progress bar is available in every Android version, because
 * {@link android.support.v4.app.NotificationCompat.Builder#setProgress(int, int, boolean)} has no
 * real effect for Android < 4.0
 */
public class CustomNotificationBuilder extends NotificationCompat.Builder {

    public static final String CHANNEL_ID_ERROR = "error";
    public static final String CHANNEL_ID_UPLOAD = "upload";
    public static final String CHANNEL_ID_DOWNLOAD = "download";

    /**
     * Custom view to replace the original layout of the notifications
     */
    private RemoteViews mContentView = null;

    /**
     * Fatory method.
     *
     * Instances of this class will be only returned in Android versions needing it.
     *
     * @param context       Context that will use the builder to create notifications
     * @return              An instance of this class, or of the regular
     *                      {@link NotificationCompat.Builder}, when it is good enough.
     */
    public static NotificationCompat.Builder getNotificationBuilder(Context context, String channelId) {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            return new CustomNotificationBuilder(context);
        } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            return new NotificationCompat.Builder(context, channelId);
        } else {
            return new NotificationCompat.Builder(context);
        }
    }

    /**
     * Constructor.
     *
     * @param context       Context that will use the builder to create notifications.
     */
    private CustomNotificationBuilder(Context context) {
        super(context);
        mContentView = new RemoteViews(context.getPackageName(), R.layout.notification_bar_progress_layout);
        setContent(mContentView);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public NotificationCompat.Builder setProgress(int max, int progress, boolean indeterminate) {
        mContentView.setProgressBar(R.id.progress, max, progress, indeterminate);
        if (max > 0) {
            mContentView.setViewVisibility(R.id.progressHolder, View.VISIBLE);
        } else {
            mContentView.setViewVisibility(R.id.progressHolder, View.GONE);
        }
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public NotificationCompat.Builder setSmallIcon(int icon) {
        super.setSmallIcon(icon);   // necessary
        mContentView.setImageViewResource(R.id.icon, icon);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public NotificationCompat.Builder setContentTitle(CharSequence title) {
        super.setContentTitle(title);
        mContentView.setTextViewText(R.id.title, title);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public NotificationCompat.Builder setContentText(CharSequence text) {
        super.setContentText(text);
        mContentView.setTextViewText(R.id.text, text);
        if (text != null && text.length() > 0) {
            mContentView.setViewVisibility(R.id.text, View.VISIBLE);
        } else {
            mContentView.setViewVisibility(R.id.text, View.GONE);
        }
        return this;
    }

    @Override
    public Notification build() {
        Notification result = super.build();
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB) {
            // super.build() in Android 2.x totally ruins whatever was made #setContent
            result.contentView = mContentView;
        }
        return result;
    }


}
