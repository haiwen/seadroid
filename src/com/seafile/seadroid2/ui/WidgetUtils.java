package com.seafile.seadroid2.ui;

import android.app.Notification;
import android.app.NotificationManager;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.support.v4.app.FragmentActivity;
import android.util.Log;
import android.webkit.MimeTypeMap;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.gallery.Util;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.activity.MarkdownActivity;
import com.seafile.seadroid2.ui.dialog.OpenAsDialog;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.text.DateFormat;

/**
 * Activity Utils
 */
public class WidgetUtils {

    public static void showFile(final FragmentActivity activity, File file) {

        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();

        if (suffix.length() == 0) {
            ToastUtils.show(activity, R.string.unknown_file_type);
            return;
        }

        // Open markdown files in MarkdownActivity
        if (suffix.endsWith("md") || suffix.endsWith("markdown")) {
            startMarkdownActivity(activity, file.getPath());
            activity.finish();
            activity.overridePendingTransition(0, 0);
            return;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        Intent open = new Intent(Intent.ACTION_VIEW);
        open.setDataAndType((Uri.fromFile(file)), mime);

        try {
            activity.startActivity(open);
            activity.finish();
            return;
        } catch (ActivityNotFoundException e) {
            new OpenAsDialog(file) {
                @Override
                public void onDismiss(DialogInterface dialog) {
                    activity.finish();
                }
            }.show(activity.getSupportFragmentManager(), "OpenAsDialog");
            return;
        }

    }

    public static void showRepo(Context context, String repoID, String repoName, String path, String dirID) {
        Intent intent = new Intent(context, BrowserActivity.class);
        intent.putExtra("repoID", repoID);
        intent.putExtra("repoName", repoName);
        intent.putExtra("path", path);
        intent.putExtra("dirID", dirID);
        context.startActivity(intent);
    }

    public static void startMarkdownActivity(Context context, String path) {
        Intent intent = new Intent(context, MarkdownActivity.class);
        intent.putExtra("path", path);
        context.startActivity(intent);
    }

    public static int getThumbnailWidth() {
        return (int) SeadroidApplication.getAppContext().getResources().getDimension(R.dimen.lv_icon_width);
    }

    public static DisplayImageOptions iconOptions = new DisplayImageOptions.Builder()
            .delayBeforeLoading(0)
            .resetViewBeforeLoading(true)
            .cacheInMemory(true)
            .cacheOnDisk(false)
            .build();

    public static final int NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID = 1;

    public static void notifyDownloadProgress(Notification notification, NotificationManager notificationManager, String repoName, String dir, int totalCount, int downloadingCount, long totalSize, long downloadedSize) {
        Log.d("WidgetUtils", "downloading " + (totalCount - downloadingCount) + "/" + totalCount + "   " + Utils.readableFileSize(downloadedSize) + "/" + Utils.readableFileSize(totalSize));
        if (totalCount == 0) {
            notificationManager.cancel(NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID);
            return;
        }
        if (downloadingCount == 0) {
            notification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    SeadroidApplication.getAppContext().getString(R.string.notification_bar_downloading_files_complete));
            notification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_done);
            notification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            notification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            notification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    Utils.readableFileSize(totalSize) + "/" + Utils.readableFileSize(totalSize));
            notification.contentView.setProgressBar(R.id.notification_bar_progressbar, 100, 100, false);
            notification.flags = Notification.FLAG_AUTO_CANCEL;
        } else if (downloadingCount > 0) {
            notification.contentView.setTextViewText(R.id.notification_bar_title_tv,
                    String.format(SeadroidApplication.getAppContext().getString(R.string.notification_bar_downloading_files_info), totalCount - downloadingCount, totalCount));
            notification.contentView.setImageViewResource(R.id.notification_bar_icon_iv, R.drawable.notification_bar_downloading);
            notification.contentView.setTextViewText(R.id.notification_bar_time_tv, Utils.getCurrentHourMinute());
            notification.contentView.setTextViewText(R.id.notification_bar_directory_info_tv, Utils.pathJoin(repoName, dir));
            notification.contentView.setTextViewText(R.id.notification_bar_size_info_tv,
                    Utils.readableFileSize(downloadedSize) + "/" + Utils.readableFileSize(totalSize));
            notification.contentView.setProgressBar(R.id.notification_bar_progressbar, 100, (int) (downloadedSize * 100 / totalSize), false);
            notification.flags = Notification.FLAG_ONGOING_EVENT;
        }

        notificationManager.notify(NOTIFICATION_DOWNLOAD_PROGRESS_BAR_ID, notification);
    }

}
