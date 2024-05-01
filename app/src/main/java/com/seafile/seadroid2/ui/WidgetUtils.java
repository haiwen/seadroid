package com.seafile.seadroid2.ui;

import static android.content.Context.NOTIFICATION_SERVICE;
import static android.content.Intent.FLAG_ACTIVITY_NEW_TASK;

import android.app.DownloadManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Build;
import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.core.app.NotificationCompat;
import androidx.core.content.FileProvider;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.markdown.MarkdownActivity;
import com.seafile.seadroid2.framework.util.FileExports;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Random;

/**
 * Activity Utils
 */
public class WidgetUtils {
    public static final String MIME_ANDROID = "application/vnd.android.package-archive";


    public static void openWith(Context context, File file) {

        String suffix = FileUtils.getFileExtension(file);
        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);

        if (TextUtils.isEmpty(mime)) {
            mime = "*/*"; // forces app chooser dialog on unknown type//
        }

        Intent openIntent = new Intent(Intent.ACTION_VIEW);
        openIntent.addFlags(FLAG_ACTIVITY_NEW_TASK);

        Uri photoURI = FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
        openIntent.setDataAndType(photoURI, mime);
        openIntent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);

        boolean isAvailable = isIntentAvailable(context, openIntent);
        if (isAvailable) {
            context.startActivity(openIntent);
        } else {
            String message = String.format(context.getString(R.string.op_exception_suitable_app_not_found), mime);
            ToastUtils.showLong(message);
        }
    }

    private static boolean isIntentAvailable(Context context, Intent intent) {
        if (Build.VERSION.SDK_INT < 30) {
            return context.getPackageManager().resolveActivity(intent, 0) != null;
        }

        PackageManager pm = context.getPackageManager();
        List<ResolveInfo> list = pm.queryIntentActivities(intent, PackageManager.MATCH_DEFAULT_ONLY);
        return !list.isEmpty();
    }

    private static void showFileForAndroid(final BaseActivity activity, File file, String
            fileName) {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                ContentResolver contentResolver = activity.getContentResolver();
                FileExports.exportFileAndroid10AndAbove(fileName, MIME_ANDROID, contentResolver, file);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        int nId = new Random(10000).nextInt();
        String channelName = "seadroid-downloader";

        NotificationManager manager = (NotificationManager) activity.getSystemService(NOTIFICATION_SERVICE);
        NotificationChannel channel = new NotificationChannel(channelName, channelName, NotificationManager.IMPORTANCE_HIGH);
        manager.createNotificationChannel(channel);

        Intent intent = new Intent(DownloadManager.ACTION_VIEW_DOWNLOADS);
        intent.addFlags(FLAG_ACTIVITY_NEW_TASK);

        PendingIntent pendingIntent = PendingIntent.getActivity(activity, nId, intent, PendingIntent.FLAG_CANCEL_CURRENT | PendingIntent.FLAG_IMMUTABLE);

        Notification notification = new NotificationCompat.Builder(activity, channelName)
                .setContentTitle(fileName + " " + activity.getString(R.string.download_finished))
                .setContentText(activity.getString(R.string.open))
                .setSmallIcon(R.drawable.icon)
                .setContentIntent(pendingIntent)
                .setAutoCancel(true)
                .build();
        manager.notify(nId, notification);
    }


    public static void startMarkdownActivity(Context context, String path) {
        Intent intent = new Intent(context, MarkdownActivity.class);
        intent.putExtra("path", path);
        context.startActivity(intent);
    }

    public static int getThumbnailWidth() {
        return (int) SeadroidApplication.getAppContext().getResources().getDimension(R.dimen.lv_icon_width);
    }
}
