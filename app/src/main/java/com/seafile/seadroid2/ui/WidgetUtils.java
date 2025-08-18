package com.seafile.seadroid2.ui;

import static android.content.Context.NOTIFICATION_SERVICE;
import static android.content.Intent.FLAG_ACTIVITY_NEW_TASK;

import android.app.DownloadManager;
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
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.util.FileExports;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivity;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

/**
 * Activity Utils
 */
public class WidgetUtils {
    public static final String MIME_ANDROID = "application/vnd.android.package-archive";

    public static List<ResolveInfo> getAppsByIntent(Intent intent) {
        PackageManager pm = SeadroidApplication.getAppContext().getPackageManager();
        List<ResolveInfo> infos = pm.queryIntentActivities(intent, 0);

        // Remove seafile app from the list
        String seadroidPackageName = SeadroidApplication.getAppContext().getPackageName();
        ResolveInfo info;
        Iterator<ResolveInfo> iter = infos.iterator();
        while (iter.hasNext()) {
            info = iter.next();
            if (info.activityInfo.packageName.equals(seadroidPackageName)) {
                iter.remove();
            }
        }

        return infos;
    }

    public static void openWith(Context context, File file) {
        String suffix = FileUtils.getFileExtension(file);
        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);

        if (TextUtils.isEmpty(mime)) {
            mime = "*/*"; // forces app chooser dialog on unknown type//
        }

        if (MIME_ANDROID.equals(mime)) {
            showFileForAndroid((BaseActivity) context, file);
            return;
        }

        Intent openIntent = new Intent(Intent.ACTION_VIEW);
        openIntent.addFlags(FLAG_ACTIVITY_NEW_TASK);

        Uri uri = FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
        openIntent.setDataAndType(uri, mime);
        openIntent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION | Intent.FLAG_GRANT_PERSISTABLE_URI_PERMISSION);
        boolean isAvailable = isIntentAvailable(context, openIntent);
        if (isAvailable) {
            context.startActivity(openIntent);
        } else {
            if (TextUtils.isEmpty(suffix)) {
                suffix = mime;
            }
            String message = String.format(context.getString(R.string.op_exception_suitable_app_not_found), suffix);
            Toasts.show(message);
        }
    }

    public static void openUrlByLocalBrowser(Context context, String url) {
        Intent intent = new Intent(Intent.ACTION_VIEW);
        intent.addFlags(FLAG_ACTIVITY_NEW_TASK);
        intent.setData(Uri.parse(url));

        boolean isAvailable = isIntentAvailable(context, intent);
        if (isAvailable) {
            context.startActivity(intent);
        } else {
            Toasts.show(R.string.activity_not_found);
        }
    }

    public static boolean isIntentAvailable(Context context, Intent intent) {
        if (Build.VERSION.SDK_INT < 30) {
            return context.getPackageManager().resolveActivity(intent, 0) != null;
        }

        PackageManager pm = context.getPackageManager();
        List<ResolveInfo> list = pm.queryIntentActivities(intent, PackageManager.MATCH_DEFAULT_ONLY);
        return !list.isEmpty();
    }

    private static void showFileForAndroid(final BaseActivity activity, File file) {
        NotificationManager manager = (NotificationManager) activity.getSystemService(NOTIFICATION_SERVICE);
        NotificationChannel channel = new NotificationChannel(NotificationUtils.OPEN_APK_CHANNEL, NotificationUtils.OPEN_APK_CHANNEL, NotificationManager.IMPORTANCE_HIGH);
        manager.createNotificationChannel(channel);

        //wait
        NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(activity, NotificationUtils.OPEN_APK_CHANNEL)
                .setContentTitle(activity.getString(R.string.waiting))
                .setSmallIcon(R.drawable.icon)
                .setAutoCancel(true);

        manager.notify(NotificationUtils.NID_OPEN_APK, notificationBuilder.build());

        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                ContentResolver contentResolver = activity.getContentResolver();
                FileExports.exportFileAndroid10AndAbove(file.getName(), MIME_ANDROID, contentResolver, file);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        Intent intent = new Intent(DownloadManager.ACTION_VIEW_DOWNLOADS);
        intent.addFlags(FLAG_ACTIVITY_NEW_TASK);

        //open
        PendingIntent pendingIntent = PendingIntent.getActivity(activity, NotificationUtils.NID_OPEN_APK, intent, PendingIntent.FLAG_CANCEL_CURRENT | PendingIntent.FLAG_IMMUTABLE);
        notificationBuilder.setContentTitle(file.getName() + " " + activity.getString(R.string.download_finished))
                .setContentIntent(pendingIntent)
                .addAction(R.drawable.action_open, activity.getString(R.string.open), pendingIntent)
                .build();
        manager.notify(NotificationUtils.NID_OPEN_APK, notificationBuilder.build());
    }


    public static int getThumbnailWidth() {
        return (int) SeadroidApplication.getAppContext().getResources().getDimension(R.dimen.lv_icon_width);
    }
}
