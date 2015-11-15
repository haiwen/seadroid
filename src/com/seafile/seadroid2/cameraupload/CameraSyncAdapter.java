package com.seafile.seadroid2.cameraupload;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.AbstractThreadedSyncAdapter;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.SyncResult;
import android.database.Cursor;
import android.os.Bundle;
import android.provider.MediaStore;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ProgressMonitor;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.ui.activity.SettingsActivity;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.net.HttpURLConnection;
import java.util.List;

/**
 * Sync adapter for media upload.
 * <p/>
 * This class uploads images/videos from the gallery to the configured seafile account.
 * It is not called directly, but managed by the Android Sync Manager instead.
 */
public class CameraSyncAdapter extends AbstractThreadedSyncAdapter {
    private static final String DEBUG_TAG = "CameraSyncAdapter";

    /**
     * Per default we will upload images/videos in this bucket
     */
    private static final String CAMERA_BUCKET_NAME = "Camera";

    private ContentResolver contentResolver;
    private SettingsManager settingsMgr = SettingsManager.instance();
    private com.seafile.seadroid2.account.AccountManager manager;

    private String targetRepoId;
    private String targetRepoName;
    private String targetDir;
    private List<String> bucketList;

    private ProgressMonitor monitor = new ProgressMonitor() {
        @Override
        public void onProgressNotify(long uploaded) {
        }

        @Override
        public boolean isCancelled() {
            return CameraSyncAdapter.this.isCancelled();
        }
    };

    /**
     * Will be set to true if the current sync has been cancelled.
     */
    private boolean cancelled = false;

    /**
     * Set up the sync adapter
     */
    public CameraSyncAdapter(Context context, boolean autoInitialize) {
        super(context, autoInitialize);

        Log.d(DEBUG_TAG, "CameraSyncAdapter created.");

        contentResolver = context.getContentResolver();
        manager = new AccountManager(context);
    }

    @Override
    public void onSyncCanceled() {
        super.onSyncCanceled();
        Log.d(DEBUG_TAG, "onPerformSync will be cancelled ");
        synchronized (this) {
            cancelled = true;
        }
    }

    private boolean isCancelled() {
        synchronized (this) {
            return cancelled;
        }
    }

    /**
     * Check if repository on the server exists.
     *
     * @param dataManager
     * @return
     * @throws SeafException
     */
    private boolean validateRepository(DataManager dataManager) throws SeafException {
        List<SeafRepo> repos = dataManager.getReposFromServer();

        for (SeafRepo repo : repos) {
            if (repo.getID().equals(targetRepoId) && repo.getName().equals(targetRepoName))
                return true;
        }

        return false;
    }

    /**
     * Create all the subdirectories on the server for the buckets that are about to be uploaded.
     *
     * @param dataManager
     * @throws SeafException
     */
    private void createDirectories(DataManager dataManager) throws SeafException {
        List<GalleryBucketUtils.Bucket> buckets = GalleryBucketUtils.getMediaBuckets(getContext());

        List<SeafDirent> dirs = dataManager.getDirentsFromServer(targetRepoId, targetDir);

        for (GalleryBucketUtils.Bucket bucket : buckets) {

            // only create directories for buckets the user has selected
            if (!bucketList.isEmpty() && !bucketList.contains(bucket.id)) {
                continue;
            }
            if (bucketList.isEmpty() && !bucket.name.equals(CAMERA_BUCKET_NAME)) {
                continue;
            }

            // first make sure that the bucket name exists and is a directory
            boolean found = false;
            for (SeafDirent dir : dirs) {
                if (dir.name.equals(bucket.name) && dir.isDir()) {
                    found = true;
                } else if (dir.name.equals(bucket.name) && !dir.isDir()) {
                    // there is already a file. move it away.
                    dataManager.rename(targetRepoId,
                            Utils.pathJoin(targetDir, dir.name),
                            dir.name + " (renamed by Seadroid)",
                            false);
                }
            }

            // create bucket directory, if its missing
            if (!found)
                dataManager.createNewDir(targetRepoId, targetDir, bucket.name);

            // update our cache for that server. we will use it later
            dataManager.getDirentsFromServer(targetRepoId, Utils.pathJoin(targetDir, bucket.name));
        }
    }

    @Override
    public void onPerformSync(android.accounts.Account account,
                              Bundle extras, String authority,
                              ContentProviderClient provider,
                              SyncResult syncResult) {

        synchronized (this) {
            cancelled = false;
        }

        Log.i(DEBUG_TAG, "Syncing images and video to " + account);

        // resync all media
        if (extras.getBoolean(ContentResolver.SYNC_EXTRAS_INITIALIZE)) {
            Log.i(DEBUG_TAG, "Doing a full resync");
            settingsMgr.setCameraUploadSyncStampImage(0);
            settingsMgr.setCameraUploadSyncStampVideo(0);
        }

        if (!settingsMgr.checkCameraUploadNetworkAvailable()) {
            Log.d(DEBUG_TAG, "Not syncing because of data plan restriction.");
            // treat dataPlan abort the same way as a network connection error
            syncResult.stats.numIoExceptions++;
            return;
        }

        Account seafileAccount = manager.getSeafileAccount(account);
        DataManager dataManager = new DataManager(seafileAccount);

        /**
         * this should never occur, as camera upload is supposed to be disabled once the camera upload
         * account signs out.
         */
        if (!seafileAccount.hasValidToken()) {
            Log.d(DEBUG_TAG, "This account has no auth token. Disable camera upload.");
            syncResult.stats.numAuthExceptions++;

            // we're logged out on this account. disable camera upload.
            ContentResolver.setIsSyncable(account, CameraUploadManager.AUTHORITY, 0);
            return;
        }

        // make copies so we're unaffected by sudden settings changes
        targetRepoId = settingsMgr.getCameraUploadRepoId();
        targetRepoName = settingsMgr.getCameraUploadRepoName();
        targetDir = settingsMgr.getCameraUploadDir();
        bucketList = settingsMgr.getCameraUploadBucketList();

        try {
            Log.d(DEBUG_TAG, "Validating target repository...");

            // make sure the repo exists
            if (!validateRepository(dataManager)) {
                /**
                 * this is a HARD error (see below). The user has to fix up the settings to make it
                 * work again.
                 *
                 * We do /not/ disable camera upload, as this might hide the problem from the user.
                 * instead we should display an error
                 */
                Log.e(DEBUG_TAG, "Sync aborted because the target repository does not exist");
                syncResult.databaseError = true;

                showNotificationRepoError();
                return;
            }

            uploadImages(syncResult, dataManager);

            if (settingsMgr.isVideosUploadAllowed()) {
                uploadVideos(syncResult, dataManager);
            }

            Log.i(DEBUG_TAG, "sync finished successfully.");

        } catch (SeafException e) {
            switch (e.getCode()) {
                /*
                 * here we have basically two scenarios
                 * SOFT) the error can be resolved without user interaction
                 *    --> mark syncResult as SOFT error (eg. stats.numIoExceptions) and return.
                 *    --> SyncManager will retry later
                 * HARD) the error can ONLY be resolved by the user performing actions on the device
                 *    --> mark syncResult as HARD error and give user a popup information.
                 */
                case HttpURLConnection.HTTP_UNAUTHORIZED:
                    // hard error -> we cannot recover without user interaction
                    syncResult.stats.numAuthExceptions++;
                    Log.i(DEBUG_TAG, "sync aborted because of authentication error.", e);
                    showNotificationAuthError();
                    break;
                default:
                    syncResult.stats.numIoExceptions++;
                    Log.i(DEBUG_TAG, "sync aborted because of IO or server-side error.", e);
                    break;
            }
        }
    }

    private void uploadImages(SyncResult syncResult, DataManager dataManager) throws SeafException {

        if (isCancelled())
            return;

        // we only want media added since the last complete sync
        String selection = MediaStore.Images.ImageColumns.DATE_ADDED + " > ?";
        String[] selectionArgs;

        if (bucketList.size() > 0) {
            // also we only want media in one of the selected buckets...
            selectionArgs = new String[bucketList.size() + 1];
            selectionArgs[0] = Long.toString(settingsMgr.getCameraUploadSyncStampImage());

            String questionmarkList = "";
            for (int i = 0; i < bucketList.size(); i++) {
                selectionArgs[i + 1] = bucketList.get(i);
                questionmarkList += "?,";
            }
            questionmarkList = questionmarkList.substring(0, questionmarkList.length() - 1); // remove last ","
            selection += " AND " + MediaStore.Images.ImageColumns.BUCKET_ID + " IN (" + questionmarkList + ")";
        } else {
            // ...or only from the Camera bucket
            selectionArgs = new String[2];
            selectionArgs[0] = Long.toString(settingsMgr.getCameraUploadSyncStampImage());
            selectionArgs[1] = CAMERA_BUCKET_NAME;
            selection += " AND " + MediaStore.Images.Media.BUCKET_DISPLAY_NAME + " = ?";
        }

        // fetch all new images from the ContentProvider since our last sync
        Cursor cursor = contentResolver.query(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                new String[]{
                        MediaStore.Images.Media._ID,
                        MediaStore.Images.Media.DATA,
                        MediaStore.Images.ImageColumns.DATE_ADDED,
                        MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME
                },
                selection,
                selectionArgs,
                MediaStore.Images.ImageColumns.DATE_ADDED + " ASC"
        );

        try {
            if (cursor.getCount() > 0) {
                Log.d(DEBUG_TAG, "i see " + cursor.getCount() + " new images.");

                // create directories for media buckets
                createDirectories(dataManager);

                iterateCursor(syncResult, dataManager, cursor);

                if (isCancelled())
                    return;

                // use the date_added of the last image as our new sync stamp
                int dateAddedColumn = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATE_ADDED);
                cursor.moveToLast();
                settingsMgr.setCameraUploadSyncStampImage(cursor.getLong(dateAddedColumn));
            }
        } finally {
            cursor.close();
        }
    }

    private void uploadVideos(SyncResult syncResult, DataManager dataManager) throws SeafException {

        if (isCancelled())
            return;


        // we only want media added since the last complete sync
        String selection = MediaStore.Video.VideoColumns.DATE_ADDED + " > ?";
        String[] selectionArgs;

        if (bucketList.size() > 0) {
            // also we only want media in one of the selected buckets...
            selectionArgs = new String[bucketList.size() + 1];
            selectionArgs[0] = Long.toString(settingsMgr.getCameraUploadSyncStampVideo());

            String questionmarkList = "";
            for (int i = 0; i < bucketList.size(); i++) {
                selectionArgs[i + 1] = bucketList.get(i);
                questionmarkList += "?,";
            }
            questionmarkList = questionmarkList.substring(0, questionmarkList.length() - 1); // remove last ","
            selection += " AND " + MediaStore.Video.VideoColumns.BUCKET_ID + " IN (" + questionmarkList + ")";
        } else {
            // ...or only from the Camera bucket
            selectionArgs = new String[2];
            selectionArgs[0] = Long.toString(settingsMgr.getCameraUploadSyncStampVideo());
            selectionArgs[1] = CAMERA_BUCKET_NAME;
            selection += " AND " + MediaStore.Video.Media.BUCKET_DISPLAY_NAME + " = ?";
        }

        // fetch all new videos from the ContentProvider since our last sync
        Cursor cursor = contentResolver.query(
                MediaStore.Video.Media.EXTERNAL_CONTENT_URI,
                new String[]{
                        MediaStore.Video.Media._ID,
                        MediaStore.Video.Media.DATA,
                        MediaStore.Video.VideoColumns.DATE_ADDED,
                        MediaStore.Video.VideoColumns.BUCKET_DISPLAY_NAME
                },
                selection,
                selectionArgs,
                MediaStore.Video.VideoColumns.DATE_ADDED + " ASC"
        );

        try {
            if (cursor.getCount() > 0) {
                Log.d(DEBUG_TAG, "i see " + cursor.getCount() + " new videos.");

                // create directories for media buckets
                createDirectories(dataManager);

                iterateCursor(syncResult, dataManager, cursor);

                if (isCancelled())
                    return;

                // use the date_added of the last image as our new sync stamp
                int dateAddedColumn = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATE_ADDED);
                cursor.moveToLast();
                settingsMgr.setCameraUploadSyncStampVideo(cursor.getLong(dateAddedColumn));
            }
        } finally {
            cursor.close();
        }

    }

    /**
     * Iterate through the content provider and upload all files
     *
     * @param syncResult
     * @param dataManager
     * @param cursor
     * @throws SeafException
     */
    private void iterateCursor(SyncResult syncResult, DataManager dataManager, Cursor cursor) throws SeafException {

        // upload them one by one
        while (!isCancelled() && cursor.moveToNext()) {

            int dataColumn = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATA);
            int bucketColumn = cursor.getColumnIndexOrThrow(MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME);

            File file = new File(cursor.getString(dataColumn));
            String bucketName = cursor.getString(bucketColumn);

            // Ignore all media by Seafile. We don't want to upload our own cached files.
            if (file.getAbsolutePath().startsWith(DataManager.getExternalRootDirectory())) {
                continue;
            }

            // local file does not exist. some inconsistency in the Media Provider? Ignore and continue
            if (!file.exists()) {
                syncResult.stats.numSkippedEntries++;
                continue;
            }

            uploadFile(dataManager, file, bucketName);
            syncResult.stats.numInserts++;

        }
    }

    /**
     * Upload a media file to the seafile server.
     *
     * @param dataManager Handle to the seafile server
     * @param file        the file to be uploaded
     * @param bucketName  the name of the media bucket
     * @throws SeafException
     */
    private void uploadFile(DataManager dataManager, File file, String bucketName) throws SeafException {

        String serverPath = Utils.pathJoin(targetDir, bucketName);

        List<SeafDirent> list = dataManager.getCachedDirents(targetRepoId, serverPath);
        if (list == null) {
            // the dirents were supposed to be refreshed in createDirectories()
            // something changed, abort.
            throw SeafException.unknownException;
        }

        for (SeafDirent dirent : list) {
            if (dirent.name.equals(file.getName()) && dirent.size == file.length()) {
                Log.d(DEBUG_TAG, "File " + file.getName() + " in bucket " + bucketName + " already exists on the server. Skipping.");
                return;
            }
        }

        //  this blocks until upload is complete
        dataManager.uploadFile(targetRepoName, targetRepoId, serverPath, file.getAbsolutePath(), monitor, false);
        Log.d(DEBUG_TAG, "upload of file " + file.getAbsolutePath() + " into bucket " + bucketName + " successful");

    }

    /**
     * Show a notification message that an auth error has occured.
     * This is something the user has to fix.
     */
    private void showNotificationAuthError() {
        NotificationCompat.Builder mBuilder =
                new NotificationCompat.Builder(getContext())
                        .setSmallIcon(R.drawable.icon)
                        .setContentTitle(getContext().getString(R.string.camera_sync_notification_title_failed))
                        .setContentText(getContext().getString(R.string.camera_sync_notification_auth_error_failed));

        // Creates an explicit intent for an Activity in your app
        Intent resultIntent = new Intent(getContext(), AccountsActivity.class);

        resultIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);

        PendingIntent dPendingIntent = PendingIntent.getActivity(getContext(),
                (int) System.currentTimeMillis(),
                resultIntent,
                0);

        mBuilder.setContentIntent(dPendingIntent);
        NotificationManager mNotificationManager =
                (NotificationManager) getContext().getSystemService(Context.NOTIFICATION_SERVICE);

        mNotificationManager.notify(0, mBuilder.build());
    }

    /**
     * Show a notification message that the server repo is missing.
     * This is something the user has to fix.
     */
    private void showNotificationRepoError() {
        NotificationCompat.Builder mBuilder =
                new NotificationCompat.Builder(getContext())
                        .setSmallIcon(R.drawable.icon)
                        .setContentTitle(getContext().getString(R.string.camera_sync_notification_title_failed))
                        .setContentText(getContext().getString(R.string.camera_sync_notification_repo_missing_failed));

        // Creates an explicit intent for an Activity in your app
        Intent resultIntent = new Intent(getContext(), SettingsActivity.class);

        resultIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);

        PendingIntent dPendingIntent = PendingIntent.getActivity(getContext(),
                (int) System.currentTimeMillis(),
                resultIntent,
                0);

        mBuilder.setContentIntent(dPendingIntent);
        NotificationManager mNotificationManager =
                (NotificationManager) getContext().getSystemService(Context.NOTIFICATION_SERVICE);

        mNotificationManager.notify(0, mBuilder.build());
    }

}
