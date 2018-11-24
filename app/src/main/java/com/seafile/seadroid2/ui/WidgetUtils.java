package com.seafile.seadroid2.ui;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.support.v4.content.FileProvider;
import android.text.ClipboardManager;
import android.webkit.MimeTypeMap;
import android.widget.Toast;

import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.activity.GalleryActivity;
import com.seafile.seadroid2.ui.activity.MarkdownActivity;
import com.seafile.seadroid2.ui.dialog.AppChoiceDialog;
import com.seafile.seadroid2.ui.dialog.GetShareLinkDialog;
import com.seafile.seadroid2.ui.dialog.GetShareLinkEncryptDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.List;

/**
 * Activity Utils
 */
public class WidgetUtils {

    public static void chooseShareApp(final BaseActivity activity,
                                      final String repoID,
                                      final String path,
                                      final boolean isdir,
                                      final Account account,
                                      final String password,
                                      final String days) {
        final Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");

        // Get a list of apps
        List<ResolveInfo> infos = Utils.getAppsByIntent(shareIntent);

        String title = activity.getString(isdir ? R.string.share_dir_link : R.string.share_file_link);

        AppChoiceDialog dialog = new AppChoiceDialog();
        dialog.addCustomAction(0, activity.getResources().getDrawable(R.drawable.copy_link),
                activity.getString(R.string.copy_link));
        dialog.init(title, infos, new AppChoiceDialog.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(AppChoiceDialog.CustomAction action) {
                final GetShareLinkDialog gdialog = new GetShareLinkDialog();
                gdialog.init(repoID, path, isdir, account, password, days);
                gdialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
                    @Override
                    @SuppressWarnings("deprecation")
                    public void onTaskSuccess() {
                        ClipboardManager clipboard = (ClipboardManager)
                                activity.getSystemService(Context.CLIPBOARD_SERVICE);
                        clipboard.setText(gdialog.getLink());
                        // ClipData clip = ClipData.newPlainText("seafile shared link", gdialog.getLink());
                        // clipboard.setPrimaryClip(clip);
                        Toast.makeText(activity, R.string.link_ready_to_be_pasted, Toast.LENGTH_SHORT).show();
                    }
                });
                gdialog.show(activity.getSupportFragmentManager(), "DialogFragment");
            }

            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                shareIntent.setClassName(packageName, className);

                final GetShareLinkDialog gdialog = new GetShareLinkDialog();
                gdialog.init(repoID, path, isdir, account,password, days);
                gdialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
                    @Override
                    public void onTaskSuccess() {
                        shareIntent.putExtra(Intent.EXTRA_TEXT, gdialog.getLink());
                        activity.startActivity(shareIntent);
                    }
                });
                gdialog.show(activity.getSupportFragmentManager(), "DialogFragment");
            }

        });
        dialog.show(activity.getSupportFragmentManager(), BrowserActivity.CHOOSE_APP_DIALOG_FRAGMENT_TAG);
    }

    public static void inputSharePassword(final BaseActivity activity,
                                          final String repoID,
                                          final String path,
                                          final boolean isdir,
                                          final Account account){
        final GetShareLinkEncryptDialog dialog = new GetShareLinkEncryptDialog();
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                String password = dialog.getPassword();
                String days = dialog.getDays();
                chooseShareApp(activity,repoID,path,isdir,account,password,days);
            }
        });
        dialog.show(activity.getSupportFragmentManager(),BrowserActivity.CHARE_LINK_PASSWORD_FRAGMENT_TAG);

    }

    /**
     * if dir will share dir link .
     * if local file ,will share file to wachat app.
     * if server file , it will download file and share file.
     *
     * @param activity
     * @param account
     * @param repoID
     * @param path
     * @param fileName
     * @param fileSize
     * @param isdir
     */
    public static void ShareWeChat(final BaseActivity activity, Account account, String repoID, String path,
                                   String fileName,
                                   long fileSize,
                                   boolean isdir) {

        if (isdir) {//share  link
            final Intent shareIntent = new Intent();
            shareIntent.setAction(Intent.ACTION_SEND);
            shareIntent.setType("text/plain");
            ResolveInfo weChatInfo = Utils.getWeChatIntent(shareIntent);
            if (weChatInfo == null) {
                activity.showShortToast(activity, R.string.no_app_available);
                return;
            }
            String className = weChatInfo.activityInfo.name;
            String packageName = weChatInfo.activityInfo.packageName;
            shareIntent.setClassName(packageName, className);
            final GetShareLinkDialog gdialog = new GetShareLinkDialog();
            gdialog.init(repoID, path, isdir, account, null, null);
            gdialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
                @Override
                public void onTaskSuccess() {
                    shareIntent.putExtra(Intent.EXTRA_TEXT, gdialog.getLink());
                    activity.startActivity(shareIntent);
                }
            });
            gdialog.show(activity.getSupportFragmentManager(), "DialogFragment");
        } else {//share  files
            BrowserActivity browserActivity = ((BrowserActivity) activity);
            String repoName = ((BrowserActivity) activity).getNavContext().getRepoName();
            String dirPath = ((BrowserActivity) activity).getNavContext().getDirPath();

            String fullPath = Utils.pathJoin(dirPath, fileName);
            final File file = browserActivity.getDataManager().getLocalRepoFile(repoName, repoID, fullPath);
            Uri uri = null;
            if (android.os.Build.VERSION.SDK_INT > 23) {
                uri = FileProvider.getUriForFile(activity, activity.getApplicationContext().getPackageName() + ".provider", file);
            } else {
                uri = Uri.fromFile(file);
            }
            final Intent sendIntent = new Intent();
            sendIntent.setAction(Intent.ACTION_SEND);
            sendIntent.setType(Utils.getFileMimeType(file));
            sendIntent.putExtra(Intent.EXTRA_STREAM, uri);
            ResolveInfo weChatInfo = Utils.getWeChatIntent(sendIntent);
            if (weChatInfo == null) {
                activity.showShortToast(activity, R.string.no_app_available);
                return;
            }
            String className = weChatInfo.activityInfo.name;
            String packageName = weChatInfo.activityInfo.packageName;
            sendIntent.setClassName(packageName, className);
            if (!Utils.isNetworkOn() && file.exists()) {
                activity.startActivity(sendIntent);
                return;
            }
            browserActivity.fetchFileAndExport(weChatInfo, sendIntent, repoName, repoID, path, fileSize);

        }
    }

    public static void showFile(final BaseActivity activity, File file) {
        showFile(activity, file, false);
    }

    /**
     * display the file according to its file type
     *
     * @param file
     */
    public static void showFile(final BaseActivity activity, File file, boolean isOpenWith) {

        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();

        //Open markdown and txt files in MarkdownActivity
        boolean isTextMime = Utils.isTextMimeType(name);
        if (isTextMime && !isOpenWith) {
            startMarkdownActivity(activity, file.getPath());
            activity.overridePendingTransition(0, 0);
            return;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        if (mime == null && isTextMime) {
            mime = "text/*"; // set .md  .markdown .txt file type//
        } else if (mime == null) {
            mime = "*/*"; // forces app chooser dialog on unknown type//
        }
        Intent open = new Intent(Intent.ACTION_VIEW);
        open.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

        if (android.os.Build.VERSION.SDK_INT > 23) {
            Uri photoURI = FileProvider.getUriForFile(activity, activity.getApplicationContext().getPackageName() + ".provider", file);
            open.setDataAndType(photoURI, mime);
            open.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
        } else {
            open.setDataAndType((Uri.fromFile(file)), mime);
        }

        if (activity.getPackageManager().resolveActivity(open, 0) == null) {
            String message = String.format(activity.getString(R.string.op_exception_suitable_app_not_found), mime);
            activity.showShortToast(activity, message);
            mime = "*/*";
            open.setType(mime);
        }

        try {
            activity.startActivity(open);
        } catch (ActivityNotFoundException e) {
            e.printStackTrace();
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

    /**
     * start and pass data to {@link GalleryActivity}
     *
     * @param repoId
     * @param path
     * @param fileName
     * @param account
     */
    public static void startGalleryActivity(Activity activity, String repoName, String repoId, String path, String fileName, Account account) {
        Intent intent = new Intent(activity, GalleryActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoId", repoId);
        intent.putExtra("path", path);
        intent.putExtra("account", account);
        intent.putExtra("fileName", fileName);
        activity.startActivity(intent);
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

}
