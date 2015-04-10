package com.seafile.seadroid2.ui;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.widget.RemoteViews;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.ui.activity.TransferActivity;
import com.seafile.seadroid2.util.Utils;

/**
 * Notification Utility class
 *
 */
public class NotificationUtils extends Notification {

    public static final int NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID = 1;
    public static final int NOTIFICATION_UPLOAD_PROGRESS_BAR_ID = 2;
    public static final String NOTIFICATION_MESSAGE_KEY = "notification message key";
    public static final String NOTIFICATION_OPEN_DOWNLOAD_TAB = "open download tab notification";
    public static final String NOTIFICATION_OPEN_UPLOAD_TAB = "open upload tab notification";

    private static Notification downloadNotification;
    private static Notification uploadNotification;
    private static NotificationManager notificationManager;
    private static Context ctx;
    
    static {
        ctx = SeadroidApplication.getAppContext();
        notificationManager = (NotificationManager) ctx.getSystemService(ctx.NOTIFICATION_SERVICE);
        buildUploadNotification();
        buildDownloadNotification();
    }

    private NotificationUtils() {
    }

    /** upload notification */
    private static void buildUploadNotification() {
        Intent uIntent = new Intent(ctx, TransferActivity.class);
        uIntent.putExtra(NOTIFICATION_MESSAGE_KEY,
                NOTIFICATION_OPEN_UPLOAD_TAB);
        uIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        uIntent.addFlags(Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS);
        uIntent.addFlags(Intent.FLAG_FROM_BACKGROUND);
        PendingIntent uPendingIntent = PendingIntent.getActivity(ctx,
                (int) System.currentTimeMillis(),
                uIntent,
                0);
        uploadNotification = new Notification(R.drawable.notification_bar_uploading,
                ctx.getString(R.string.notification_upload_started_title),
                System.currentTimeMillis());
        uploadNotification.flags |= Notification.FLAG_AUTO_CANCEL;
        uploadNotification.contentView = new RemoteViews(ctx.getPackageName(),
                R.layout.notification_bar_progress_layout);
        uploadNotification.contentIntent = uPendingIntent;
    }

    /** download notification */
    public static void buildDownloadNotification() {
        Intent dIntent = new Intent(ctx, TransferActivity.class);
        dIntent.putExtra(NOTIFICATION_MESSAGE_KEY,
                NOTIFICATION_OPEN_DOWNLOAD_TAB);
        dIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        dIntent.addFlags(Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS);
        dIntent.addFlags(Intent.FLAG_FROM_BACKGROUND);
        PendingIntent dPendingIntent = PendingIntent.getActivity(ctx,
                (int) System.currentTimeMillis(),
                dIntent,
                0);
        downloadNotification = new Notification(R.drawable.notification_bar_downloading,
                ctx.getString(R.string.notification_download_started_title),
                System.currentTimeMillis());
        downloadNotification.flags |= Notification.FLAG_AUTO_CANCEL;
        downloadNotification.contentView = new RemoteViews(ctx.getPackageName(),
                R.layout.notification_bar_progress_layout);
        downloadNotification.contentIntent = dPendingIntent;
    }

    public static void notifyUploadProgress(String repoName,
                                            String dir,
                                            int totalCount,
                                            int uploadingCount,
                                            long totalSize,
                                            long uploadedSize) {
        String strUplodedSize = Utils.readableFileSize(uploadedSize);
        String strTotalSize = Utils.readableFileSize(totalSize);

        if (totalCount == 0) {
            notificationManager.cancel(NOTIFICATION_UPLOAD_PROGRESS_BAR_ID);
            return;
        }

        // uploading
        if (uploadingCount > 0) {
            long progress = uploadedSize * 100 / totalSize;
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    String.format(SeadroidApplication.getAppContext().getResources().getQuantityString(R.plurals.notification_upload_info,
                                    uploadingCount),
                            uploadingCount,
                            progress));
            uploadNotification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_uploading);
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    strUplodedSize + "/" + strTotalSize);
            uploadNotification.flags = Notification.FLAG_ONGOING_EVENT;
        }

        // upload completed
        if (uploadingCount == 0 && strTotalSize.equals(strUplodedSize) && totalSize > 0) {
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    SeadroidApplication.getAppContext().getString(R.string.notification_upload_completed));
            uploadNotification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_done);
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    strUplodedSize + "/" + strTotalSize);
            uploadNotification.flags = Notification.FLAG_AUTO_CANCEL;
        }

        // upload cancelled
        if (uploadingCount == 0 && !strTotalSize.equals(strUplodedSize) && totalSize > 0) {
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    SeadroidApplication.getAppContext().getString(R.string.notification_upload_cancelled));
            uploadNotification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_uploading);
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            uploadNotification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    strUplodedSize + "/" + strTotalSize);
            uploadNotification.flags = Notification.FLAG_AUTO_CANCEL;
            notificationManager.cancel(NOTIFICATION_UPLOAD_PROGRESS_BAR_ID);
            return;
        }

        notificationManager.notify(NOTIFICATION_UPLOAD_PROGRESS_BAR_ID, uploadNotification);
    }

    public static void notifyDownloadProgress(String repoName,
                                              String dir,
                                              int totalCount,
                                              int downloadingCount,
                                              long totalSize,
                                              long downloadedSize) {
        String strDownlodedSize = Utils.readableFileSize(downloadedSize);
        String strTotalSize = Utils.readableFileSize(totalSize);

        if (totalCount == 0) {
            notificationManager.cancel(NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID);
            return;
        }

        // downloading
        if (downloadingCount > 0) {
            long progress = 100;
            if (totalSize != 0) {
                progress = downloadedSize * 100 / totalSize;
            }
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    String.format(SeadroidApplication.getAppContext().getResources().getQuantityString(R.plurals.notification_download_info, downloadingCount),
                            downloadingCount,
                            progress));
            downloadNotification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_downloading);
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    strDownlodedSize + "/" + strTotalSize);
            downloadNotification.flags = Notification.FLAG_ONGOING_EVENT;
        }

        // download completed
        // downloadedSize doesn`t equal totalSize even when download completed, actually they are like 21571614/21571617 (downloadedSize/totalSize)
        if (downloadingCount == 0 && strDownlodedSize.equals(strTotalSize) && totalSize > 0) {
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    SeadroidApplication.getAppContext().getString(R.string.notification_download_completed));
            downloadNotification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_done);
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    strDownlodedSize + "/" + strTotalSize);
            downloadNotification.flags = Notification.FLAG_AUTO_CANCEL;
        }

        // download cancelled
        if (downloadingCount == 0 && !strDownlodedSize.equals(strTotalSize) && totalSize > 0) {
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    SeadroidApplication.getAppContext().getString(R.string.notification_download_cancelled));
            downloadNotification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_downloading);
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            downloadNotification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    strDownlodedSize + "/" + strTotalSize);
            downloadNotification.flags = Notification.FLAG_AUTO_CANCEL;
            notificationManager.cancel(NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID);
            return;
        }

        notificationManager.notify(NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID, downloadNotification);
    }

    public static void cancelUpoloadNotificaiton() {
        notificationManager.cancel(NOTIFICATION_UPLOAD_PROGRESS_BAR_ID);
    }

    public static void cancelDownloadNotificaiton() {
        notificationManager.cancel(NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID);
    }
}
