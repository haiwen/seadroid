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
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;

import com.blankj.utilcode.util.ClipboardUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.objs.DirentShareLinkModel;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.util.FileExports;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.listener.OnCreateDirentShareLinkListener;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.dialog_fragment.AppChoiceDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.GetShareLinkPasswordDialogFragment;

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



    public static void showChooseAppDialog(Context context, FragmentManager fragmentManager, DirentShareLinkModel shareLinkModel, boolean isDir) {
        String title = context.getString(isDir ? R.string.share_dir_link : R.string.share_file_link);

        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");
        List<ResolveInfo> infos = WidgetUtils.getAppsByIntent(shareIntent);

        AppChoiceDialogFragment dialog = new AppChoiceDialogFragment();
        dialog.addCustomAction(0,
                ContextCompat.getDrawable(context, R.drawable.copy_link),
                context.getString(R.string.copy_link));
        dialog.init(title, infos, new AppChoiceDialogFragment.OnItemSelectedListener() {
            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                shareIntent.setClassName(packageName, className);
                shareIntent.putExtra(Intent.EXTRA_TEXT, shareLinkModel.link);
                context.startActivity(shareIntent);
                dialog.dismiss();
            }

            @Override
            public void onCustomActionSelected(AppChoiceDialogFragment.CustomAction action) {
                ClipboardUtils.copyText(shareLinkModel.link);
                Toasts.show(R.string.link_ready_to_be_pasted);
                dialog.dismiss();
            }
        });
        dialog.show(fragmentManager, AppChoiceDialogFragment.class.getSimpleName());
    }

    public static void showCreateShareLinkDialog(Context context, FragmentManager fragmentManager, DirentModel direntModel, boolean isAdvance) {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            return;
        }

        if (direntModel == null) {
            return;
        }

        GetShareLinkPasswordDialogFragment dialogFragment = new GetShareLinkPasswordDialogFragment();
        dialogFragment.init(direntModel.repo_id, direntModel.full_path, isAdvance);
        dialogFragment.setOnCreateDirentShareLinkListener(new OnCreateDirentShareLinkListener() {
            @Override
            public void onCreateDirentShareLink(DirentShareLinkModel linkModel) {
                if (linkModel == null) {
                    dialogFragment.dismiss();
                    return;
                }
                showChooseAppDialog(context, fragmentManager, linkModel, direntModel.isDir());
                dialogFragment.dismiss();
            }
        });
        dialogFragment.show(fragmentManager, GetShareLinkPasswordDialogFragment.class.getSimpleName());
    }

    private static ResolveInfo getWeChatIntent(Intent intent) {
        PackageManager pm = SeadroidApplication.getAppContext().getPackageManager();
        List<ResolveInfo> infos = pm.queryIntentActivities(intent, 0);
        for (ResolveInfo info : infos) {
            if (info.activityInfo.packageName.equals("com.tencent.mm")) {
                return info;
            }
        }

        return null;
    }

    /**
     * share link to wechat
     */
    public static void shareDirToWeChat(Fragment context, String repo_id, String full_path) {
        Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");

        ResolveInfo weChatInfo = getWeChatIntent(shareIntent);
        if (weChatInfo == null) {
            Toasts.show(R.string.no_app_available);
            return;
        }

        String className = weChatInfo.activityInfo.name;
        String packageName = weChatInfo.activityInfo.packageName;
        shareIntent.setClassName(packageName, className);

        GetShareLinkPasswordDialogFragment dialogFragment = new GetShareLinkPasswordDialogFragment();
        dialogFragment.init(repo_id, full_path, false);
        dialogFragment.setOnCreateDirentShareLinkListener(new OnCreateDirentShareLinkListener() {
            @Override
            public void onCreateDirentShareLink(DirentShareLinkModel linkModel) {
                if (linkModel == null) {
                    dialogFragment.dismiss();
                    return;
                }

                shareIntent.putExtra(Intent.EXTRA_TEXT, linkModel.link);
                context.startActivity(shareIntent);
                dialogFragment.dismiss();
            }
        });
        dialogFragment.show(context.getChildFragmentManager(), GetShareLinkPasswordDialogFragment.class.getSimpleName());
    }

    /**
     * share file to wachat
     */
    public static void shareFileToWeChat(Fragment context, File file) {

        Uri uri = FileProvider.getUriForFile(context.requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        ResolveInfo weChatInfo = getWeChatIntent(sendIntent);
        if (weChatInfo == null) {
            Toasts.show(R.string.no_app_available);
            return;
        }

        String className = weChatInfo.activityInfo.name;
        String packageName = weChatInfo.activityInfo.packageName;
        sendIntent.setClassName(packageName, className);
        context.startActivity(sendIntent);
    }

    /**
     * Export a file.
     * 1. first ask the user to choose an app
     * 2. then download the latest version of the file
     * 3. start the choosen app
     */
    public static void exportFile(Fragment context, File localFile) {
        Uri uri = FileProvider.getUriForFile(context.requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, localFile);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(localFile));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = WidgetUtils.getAppsByIntent(sendIntent);
        if (infos.isEmpty()) {
            Toasts.show(R.string.no_app_available);
            return;
        }

        AppChoiceDialogFragment dialog = new AppChoiceDialogFragment();
        dialog.init(context.getString(R.string.export_file), infos, new AppChoiceDialogFragment.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(AppChoiceDialogFragment.CustomAction action) {
            }

            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                sendIntent.setClassName(packageName, className);

                context.startActivity(sendIntent);
            }
        });
        dialog.show(context.getChildFragmentManager(), AppChoiceDialogFragment.class.getSimpleName());
    }
}
