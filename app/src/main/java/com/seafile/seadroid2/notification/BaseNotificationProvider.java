package com.seafile.seadroid2.notification;

import android.app.NotificationManager;
import android.support.v4.app.NotificationCompat;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.transfer.TransferManager;
import com.seafile.seadroid2.transfer.TransferService;

import java.util.Timer;
import java.util.TimerTask;

/**
 * All downloading events will be represented by one downloading notification, at the same time all
 * uploading events will be represented by one uploading notification as well.
 * maintain state of downloading or uploading events and update the relevant notification.
 */
public abstract class BaseNotificationProvider {

    protected NotificationCompat.Builder mNotifBuilder;

    protected NotificationManager mNotifMgr = (NotificationManager) SeadroidApplication.getAppContext().
            getSystemService(SeadroidApplication.getAppContext().NOTIFICATION_SERVICE);

    public static final String NOTIFICATION_MESSAGE_KEY = "notification message key";
    /** Creates an explicit flag for opening @{link com.seafile.seadroid2.ui.fragment.DownloadTaskFragment}
     * in @{link com.seafile.seadroid2.ui.activity.TransferActivity} */
    public static final String NOTIFICATION_OPEN_DOWNLOAD_TAB = "open download tab notification";
    /** Creates an explicit flag for opening @{link com.seafile.seadroid2.ui.fragment.UploadTaskFragment}
     * in @{link com.seafile.seadroid2.ui.activity.TransferActivity} */
    public static final String NOTIFICATION_OPEN_UPLOAD_TAB = "open upload tab notification";

    public static final int NOTIFICATION_ID_DOWNLOAD = 1;
    public static final int NOTIFICATION_ID_UPLOAD = 2;
    public static final int NOTIFICATION_ID_MEDIA = 3;

    protected TransferManager txMgr;
    protected TransferService txService;

    public BaseNotificationProvider(TransferManager transferManager,
                                    TransferService transferService) {
        this.txMgr = transferManager;
        this.txService = transferService;
    }

    /**
     * calculate state
     *
     * @return
     *        {@code NotificationState.NOTIFICATION_STATE_FAILED}, when at least one task failed
     *        {@code NotificationState.NOTIFICATION_STATE_CANCELLED}, when at least one task cancelled
     *        {@code NotificationState.NOTIFICATION_STATE_PROGRESS}, when at least one task in progress
     *        {@code NotificationState.NOTIFICATION_STATE_COMPLETED}, otherwise.
     */
    protected abstract NotificationState getState();

    /**
     * get notification id
     *
     * @return
     *          notificationID
     */
    protected abstract int getNotificationID();

    /**
     * get notification title texts
     *
     * @return
     *          some descriptions shown in notification title
     */
    protected abstract String getNotificationTitle();

    /**
     * update notification
     */
    public void updateNotification() {
        if (mNotifBuilder == null)
            notifyStarted();

        String progressInfo = getProgressInfo();
        String notifTitle = getNotificationTitle();
        int notifId = getNotificationID();
        int progress = getProgress();

        if (getState().equals(NotificationState.NOTIFICATION_STATE_PROGRESS)) {
            notifyProgress(notifId, notifTitle, progressInfo, progress);
        } else if (getState().equals(NotificationState.NOTIFICATION_STATE_COMPLETED_WITH_ERRORS)) {
            notifyCompletedWithErrors(notifId, notifTitle, progressInfo, progress);
        } else if (getState().equals(NotificationState.NOTIFICATION_STATE_COMPLETED)) {
            notifyCompleted(notifId, notifTitle, progressInfo);
        }
    }

    /**
     * start to show a notification
     *
     */
    protected abstract void notifyStarted();

    /**
     * update notification when downloading or uploading in progress
     *
     * @param notificationID
     *          use to update the notification later on
     * @param title
     *          some descriptions shown in notification title
     * @param info
     *          some descriptions to indicate the upload status
     * @param progress
     *          progress value to update build-in progressbar
     *
     */
    private void notifyProgress(int notificationID,
                                String title,
                                String info,
                                int progress) {
        mNotifBuilder.setSmallIcon(R.drawable.icon);
        mNotifBuilder.setContentTitle(title);
        mNotifBuilder.setContentText(info);
        mNotifBuilder.setProgress(100, progress, false);
        mNotifMgr.notify(notificationID, mNotifBuilder.build());
    }

    /**
     * update notification when completed
     *
     * @param notificationID
     *          use to update the notification later on
     * @param title
     *          some descriptions shown in notification title
     *
     */
    private void notifyCompleted(int notificationID, String title, String info) {
        mNotifBuilder.setSmallIcon(R.drawable.icon);
        mNotifBuilder.setContentTitle(title);
        mNotifBuilder.setContentText(info);
        mNotifBuilder.setProgress(100, 100, false);
        mNotifBuilder.setAutoCancel(true);
        mNotifBuilder.setOngoing(false);
        mNotifMgr.notify(notificationID, mNotifBuilder.build());
        mNotifBuilder = null;
        cancelWithDelay(txService, 5000);
    }

    /**
     * Delay for a while before cancel notification in order user can see the result
     *
     * @param transferService
     * @param delayInMillis
     */
    public static void cancelWithDelay(final TransferService transferService,
            long delayInMillis) {
        new Timer().schedule(new TimerTask() {
            @Override
            public void run() {
                if (!transferService.isTransferring()) {
                    transferService.stopForeground(true);
                }
            }
        }, delayInMillis);

    }

    /**
     * update notification when failed or cancelled
     *
     * @param notificationID
     *          use to update the notification later on
     * @param title
     *          some descriptions shown in notification title
     * @param info
     *          some descriptions to indicate the upload status
     * @param progress
     *          progress value to update build-in progressbar
     *
     */
    protected void notifyCompletedWithErrors(int notificationID, String title, String info, int progress) {
        mNotifBuilder.setSmallIcon(R.drawable.icon);
        mNotifBuilder.setContentTitle(title);
        mNotifBuilder.setContentText(info);
        mNotifBuilder.setProgress(100, progress, false);
        mNotifBuilder.setAutoCancel(true);
        mNotifBuilder.setOngoing(false);
        mNotifMgr.notify(notificationID, mNotifBuilder.build());
        mNotifBuilder = null;
        cancelWithDelay(txService, 5000);
    }

    /**
     * get downloading or uploading status
     *
     * @return
     *         texts of downloading or uploading status
     */
    protected abstract String getProgressInfo();

    /**
     * get progress of transferred files
     *
     * @return
     *          progress
     */
    protected abstract int getProgress();

    /**
     * Clear notification from notification area
     */
    public void cancelNotification() {
        mNotifMgr.cancelAll();
        mNotifBuilder = null;
        cancelWithDelay(txService, 2000);
    }

    public enum NotificationState {
        NOTIFICATION_STATE_PROGRESS,
        NOTIFICATION_STATE_COMPLETED,
        NOTIFICATION_STATE_COMPLETED_WITH_ERRORS
    }

}
