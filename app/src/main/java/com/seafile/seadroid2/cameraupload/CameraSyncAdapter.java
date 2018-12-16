package com.seafile.seadroid2.cameraupload;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.AbstractThreadedSyncAdapter;
import android.content.ComponentName;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SyncResult;
import android.database.Cursor;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.MediaStore;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

import com.google.common.base.Joiner;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.CustomNotificationBuilder;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.ui.activity.SettingsActivity;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Sync adapter for media upload.
 * <p/>
 * This class uploads images/videos from the gallery to the configured seafile account.
 * It is not called directly, but managed by the Android Sync Manager instead.
 */
public class CameraSyncAdapter extends AbstractThreadedSyncAdapter {
    private static final String DEBUG_TAG = "CameraSyncAdapter";

    private ContentResolver contentResolver;

    private SettingsManager settingsMgr = SettingsManager.instance();
    private com.seafile.seadroid2.account.AccountManager manager;
    private CameraUploadDBHelper dbHelper;

    private String targetRepoId;
    private String targetRepoName;
    private List<String> bucketList;

    private final String BASE_DIR = "My Photos";

    /**
     * Will be set to true if the current sync has been cancelled.
     */
    private boolean cancelled = false;

    /**
     * Media files we have sent over to the TransferService. Thread-safe.
     */
    private List<Integer> tasksInProgress = new ArrayList<>();

    TransferService txService = null;

    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            // this will run in a foreign thread!

            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            synchronized (CameraSyncAdapter.this) {
                txService = binder.getService();
            }
            // Log.d(DEBUG_TAG, "connected to TransferService");
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            // this will run in a foreign thread!
            // Log.d(DEBUG_TAG, "disconnected from TransferService, aborting sync");

            onSyncCanceled();
            synchronized (CameraSyncAdapter.this) {
                txService = null;
            }
        }
    };

    /**
     * Set up the sync adapter
     */
    public CameraSyncAdapter(Context context) {
        /*
         * autoInitialize is set to false because we need to handle initialization
         * ourselves in performSync() (resetting the photo database).
         */
        super(context, false);

        // Log.d(DEBUG_TAG, "CameraSyncAdapter created.");

        contentResolver = context.getContentResolver();
        manager = new AccountManager(context);
        dbHelper = CameraUploadDBHelper.getInstance();
    }

    private synchronized void startTransferService() {
        if (txService != null)
            return;

        Intent bIntent = new Intent(getContext(), TransferService.class);
        getContext().bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    public void onSyncCanceled() {
        super.onSyncCanceled();
        // Log.d(DEBUG_TAG, "onPerformSync will be cancelled ");
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

        // create base directory
        forceCreateDirectory(dataManager, "/", BASE_DIR);

        for (GalleryBucketUtils.Bucket bucket : buckets) {

            // the user has selected specific buckets: only create directories for these
            if (!bucketList.isEmpty() && !bucketList.contains(bucket.id)) {
                continue;
            }

            // auto-guessing is on: create directories for camera buckets
            if (bucketList.isEmpty() && !bucket.isCameraBucket)
                continue;

            forceCreateDirectory(dataManager, BASE_DIR, bucket.name);

            // update our cache for that server. we will use it later
            dataManager.getDirentsFromServer(targetRepoId, Utils.pathJoin(BASE_DIR, bucket.name));
        }
    }

    /**
     * Create a directory, rename a file away if necessary,
     *
     * @param dataManager
     * @param parent parent dir
     * @param dir directory to create
     * @throws SeafException
     */
    private void forceCreateDirectory(DataManager dataManager, String parent, String dir) throws SeafException {

        List<SeafDirent> dirs = dataManager.getDirentsFromServer(targetRepoId, parent);
        boolean found = false;
        for (SeafDirent dirent : dirs) {
           if (dirent.name.equals(dir) && dirent.isDir()) {
                found = true;
            } else if (dirent.name.equals(dir) && !dirent.isDir()) {
                // there is already a file. move it away.
                String newFilename = getContext().getString(R.string.camera_sync_rename_file, dirent.name);
                dataManager.rename(targetRepoId,
                        Utils.pathJoin(Utils.pathJoin("/", parent), dirent.name),
                        newFilename,
                        false);
            }
        }
        if (!found)
            dataManager.createNewDir(targetRepoId, Utils.pathJoin("/", parent), dir);
    }

    @Override
    public void onPerformSync(android.accounts.Account account,
                              Bundle extras, String authority,
                              ContentProviderClient provider,
                              SyncResult syncResult) {

        synchronized (this) {
            cancelled = false;
        }

        /*Log.i(DEBUG_TAG, "Syncing images and video to " + account);

        Log.d(DEBUG_TAG, "Selected buckets for camera upload: "+settingsMgr.getCameraUploadBucketList());
        Log.d(DEBUG_TAG, "is video upload allowed: "+settingsMgr.isVideosUploadAllowed());
        Log.d(DEBUG_TAG, "is data plan allowed: "+settingsMgr.isDataPlanAllowed());

        Log.d(DEBUG_TAG, "Media buckets available on this system: ");*/
        for (GalleryBucketUtils.Bucket bucket: GalleryBucketUtils.getMediaBuckets(getContext())) {
            // Log.d(DEBUG_TAG, "Bucket id="+bucket.id+" name="+bucket.name);
        }

        // resync all media
        if (extras.getBoolean(ContentResolver.SYNC_EXTRAS_INITIALIZE)) {
            // Log.i(DEBUG_TAG, "Doing a full resync");
            dbHelper.cleanPhotoCache();
        }

        if (!settingsMgr.checkCameraUploadNetworkAvailable()) {
            // Log.d(DEBUG_TAG, "Not syncing because of data plan restriction.");
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
            // Log.d(DEBUG_TAG, "This account has no auth token. Disable camera upload.");
            syncResult.stats.numAuthExceptions++;

            // we're logged out on this account. disable camera upload.
            ContentResolver.cancelSync(account, CameraUploadManager.AUTHORITY);
            ContentResolver.setIsSyncable(account, CameraUploadManager.AUTHORITY, 0);
            return;
        }

        // make copies so we're unaffected by sudden settings changes
        targetRepoId = settingsMgr.getCameraUploadRepoId();
        targetRepoName = settingsMgr.getCameraUploadRepoName();
        bucketList = settingsMgr.getCameraUploadBucketList();

        try {
            // Log.d(DEBUG_TAG, "Validating target repository...");

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

            // Log.d(DEBUG_TAG, "connecting to TransferService");
            startTransferService();

            // wait for TransferService to connect
            // Log.d(DEBUG_TAG, "waiting for transfer service");
            int timeout = 1000; // wait up to a second
            while (!isCancelled() && timeout > 0 && txService == null) {
                // Log.d(DEBUG_TAG, "waiting for transfer service");
                Thread.sleep(100);
                timeout -= 100;
            }

            if (txService == null) {
                Log.e(DEBUG_TAG, "TransferService did not come up in time, aborting sync");
                syncResult.delayUntil = 60;
                return;
            }

            uploadImages(syncResult, dataManager);

            if (settingsMgr.isVideosUploadAllowed()) {
                uploadVideos(syncResult, dataManager);
            }

            if (isCancelled()) {
                // Log.i(DEBUG_TAG, "sync was cancelled.");
            } else {
                // Log.i(DEBUG_TAG, "sync finished successfully.");
            }
            // Log.d(DEBUG_TAG, "syncResult: " + syncResult);

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
                    // Log.i(DEBUG_TAG, "sync aborted because of authentication error.", e);
                    showNotificationAuthError();
                    break;
                default:
                    syncResult.stats.numIoExceptions++;
                    // Log.i(DEBUG_TAG, "sync aborted because of IO or server-side error.", e);
                    break;
            }
        } catch (Exception e) {
            Log.e(DEBUG_TAG, "sync aborted because an unknown error", e);
            syncResult.stats.numParseExceptions++;
        } finally {
            if (txService != null) {

                // Log.d(DEBUG_TAG, "Cancelling remaining pending tasks (if any)");
                txService.cancelUploadTasksByIds(tasksInProgress);

                // Log.d(DEBUG_TAG, "disconnecting from TransferService");
                getContext().unbindService(mConnection);
                txService = null;
            }
        }
    }

    private void uploadImages(SyncResult syncResult, DataManager dataManager) throws SeafException, InterruptedException {

        // Log.d(DEBUG_TAG, "Starting to upload images...");

        if (isCancelled())
            return;

        List<String> selectedBuckets = new ArrayList<>();
        if (bucketList.size() > 0) {
            selectedBuckets = bucketList;
        } else {
            List<GalleryBucketUtils.Bucket> allBuckets = GalleryBucketUtils.getMediaBuckets(getContext());
            for (GalleryBucketUtils.Bucket bucket: allBuckets) {
                if (bucket.isCameraBucket)
                    selectedBuckets.add(bucket.id);
            }
        }

        String[] selectionArgs = selectedBuckets.toArray(new String[]{});
        String selection = MediaStore.Images.ImageColumns.BUCKET_ID + " IN " + varArgs(selectedBuckets.size());

        // Log.d(DEBUG_TAG, "ContentResolver selection='"+selection+"' selectionArgs='"+Arrays.deepToString(selectionArgs)+"'");

        // fetch all new images from the ContentProvider since our last sync
        Cursor cursor = contentResolver.query(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                new String[]{
                        MediaStore.Images.Media._ID,
                        MediaStore.Images.Media.DATA,
                        MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME
                },
                selection,
                selectionArgs,
                MediaStore.Images.ImageColumns.DATE_ADDED + " ASC"
        );

        try {
            if (cursor == null) {
                Log.e(DEBUG_TAG, "ContentResolver query failed!");
                return;
            }
            // Log.d(DEBUG_TAG, "i see " + cursor.getCount() + " new images.");
            if (cursor.getCount() > 0) {
                // create directories for media buckets
                createDirectories(dataManager);

                iterateCursor(syncResult, dataManager, cursor);

                if (isCancelled())
                    return;

            }
        } finally {
            if (cursor != null)
                cursor.close();
        }
    }

    private void uploadVideos(SyncResult syncResult, DataManager dataManager) throws SeafException, InterruptedException {

        // Log.d(DEBUG_TAG, "Starting to upload videos...");

        if (isCancelled())
            return;

        List<String> selectedBuckets = new ArrayList<>();
        if (bucketList.size() > 0) {
            selectedBuckets = bucketList;
        } else {
            List<GalleryBucketUtils.Bucket> allBuckets = GalleryBucketUtils.getMediaBuckets(getContext());
            for (GalleryBucketUtils.Bucket bucket: allBuckets) {
                if (bucket.isCameraBucket)
                    selectedBuckets.add(bucket.id);
            }
        }

        String[] selectionArgs = selectedBuckets.toArray(new String[]{});
        String selection = MediaStore.Video.VideoColumns.BUCKET_ID + " IN " + varArgs(selectedBuckets.size());

        // Log.d(DEBUG_TAG, "ContentResolver selection='"+selection+"' selectionArgs='"+Arrays.deepToString(selectionArgs)+"'");

        // fetch all new videos from the ContentProvider since our last sync
        Cursor cursor = contentResolver.query(
                MediaStore.Video.Media.EXTERNAL_CONTENT_URI,
                new String[]{
                        MediaStore.Video.Media._ID,
                        MediaStore.Video.Media.DATA,
                        MediaStore.Video.VideoColumns.BUCKET_DISPLAY_NAME
                },
                selection,
                selectionArgs,
                MediaStore.Video.VideoColumns.DATE_ADDED + " ASC"
        );

        try {
            if (cursor == null) {
                Log.e(DEBUG_TAG, "ContentResolver query failed!");
                return;
            }
            // Log.d(DEBUG_TAG, "i see " + cursor.getCount() + " new videos.");
            if (cursor.getCount() > 0) {
                // create directories for media buckets
                createDirectories(dataManager);

                iterateCursor(syncResult, dataManager, cursor);

                if (isCancelled())
                    return;

            }
        } finally {
            if (cursor != null)
                cursor.close();
        }

    }

    private String varArgs(int count) {
        String[] chars = new String[count];
        Arrays.fill(chars, "?");
        return "( " + Joiner.on(", ").join(chars) + " )";
    }

    /**
     * Iterate through the content provider and upload all files
     *
     * @param syncResult
     * @param dataManager
     * @param cursor
     * @throws SeafException
     */
    private void iterateCursor(SyncResult syncResult, DataManager dataManager, Cursor cursor) throws SeafException, InterruptedException {

        tasksInProgress.clear();

        // upload them one by one
        while (!isCancelled() && cursor.moveToNext()) {

            int dataColumn = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATA);
            int bucketColumn = cursor.getColumnIndexOrThrow(MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME);

            // some inconsistency in the Media Provider? Ignore and continue
            if (cursor.getString(dataColumn) == null) {
                syncResult.stats.numSkippedEntries++;
                continue;
            }

            File file = new File(cursor.getString(dataColumn));
            String bucketName = cursor.getString(bucketColumn);

            // local file does not exist. some inconsistency in the Media Provider? Ignore and continue
            if (!file.exists()) {
                // Log.d(DEBUG_TAG, "Skipping media "+file+" because it doesn't exist");
                syncResult.stats.numSkippedEntries++;
                continue;
            }

            // Ignore all media by Seafile. We don't want to upload our own cached files.
            if (file.getAbsolutePath().startsWith(StorageManager.getInstance().getMediaDir().getAbsolutePath())) {
                // Log.d(DEBUG_TAG, "Skipping media "+file+" because it's part of the Seadroid cache");
                continue;
            }

            if (dbHelper.isUploaded(file)) {
                // Log.d(DEBUG_TAG, "Skipping media " + file + " because we have uploaded it in the past.");
                continue;
            }

            uploadFile(dataManager, file, bucketName);
        }

        waitForUploads();
        checkUploadResult(syncResult);
    }

    private void waitForUploads() throws InterruptedException {
        // Log.d(DEBUG_TAG, "wait for transfer service to finish our tasks");
        WAITLOOP: while (!isCancelled()) {
            Thread.sleep(100); // wait

            for (int id: tasksInProgress) {
                UploadTaskInfo info = txService.getUploadTaskInfo(id);
                if (info.state == TaskState.INIT || info.state == TaskState.TRANSFERRING) {
                    // there is still at least one task pending
                    continue  WAITLOOP;
                }
            }
            break;
        }
    }

    /**
     * Upload is finished. Go through task infos and mark files as uploaded in our DB
     *
     * @param syncResult
     * @throws SeafException
     */
    private void checkUploadResult(SyncResult syncResult) throws SeafException {
        for (int id: tasksInProgress) {
            UploadTaskInfo info = txService.getUploadTaskInfo(id);
            if (info.err != null) {
                throw info.err;
            }
            if (info.state == TaskState.FINISHED) {
                File file = new File(info.localFilePath);
                dbHelper.markAsUploaded(file);
                syncResult.stats.numInserts++;
            } else {
                throw SeafException.unknownException;
            }
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

        String serverPath = Utils.pathJoin(BASE_DIR, bucketName);

        List<SeafDirent> list = dataManager.getCachedDirents(targetRepoId, serverPath);
        if (list == null) {
            Log.e(DEBUG_TAG, "Seadroid dirent cache is empty in uploadFile. Should not happen, aborting.");
            // the dirents were supposed to be refreshed in createDirectories()
            // something changed, abort.
            throw SeafException.unknownException;
        }

        /*
         * We don't want to upload a file twice unless the local and remote files differ.
         *
         * It would be cool if the API2 offered a way to query the hash of a remote file.
         * Currently, comparing the file size is the best we can do.
         */
        String filename = file.getName();
        String prefix = filename.substring(0, filename.lastIndexOf("."));
        String suffix = filename.substring(filename.lastIndexOf("."));
        Pattern pattern = Pattern.compile(Pattern.quote(prefix) + "( \\(\\d+\\))?" + Pattern.quote(suffix));
        for (SeafDirent dirent : list) {
            if (pattern.matcher(dirent.name).matches() && dirent.size == file.length()) {
                // Log.d(DEBUG_TAG, "File " + file.getName() + " in bucket " + bucketName + " already exists on the server. Skipping.");
                dbHelper.markAsUploaded(file);
                return;
            }
        }

        // Log.d(DEBUG_TAG, "uploading file " + file.getName() + " to " + serverPath);
        int taskID = txService.addUploadTask(dataManager.getAccount(), targetRepoId, targetRepoName,
                serverPath, file.getAbsolutePath(), false, false);
        tasksInProgress.add(taskID);
    }

    /**
     * Show a notification message that an auth error has occured.
     * This is something the user has to fix.
     */
    private void showNotificationAuthError() {
        NotificationCompat.Builder mBuilder;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            mBuilder = new NotificationCompat.Builder(getContext(), CustomNotificationBuilder.CHANNEL_ID_ERROR);
        } else {
            mBuilder = new NotificationCompat.Builder(getContext());
        }

        mBuilder.setSmallIcon(R.drawable.icon)
                .setOnlyAlertOnce(true)
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
