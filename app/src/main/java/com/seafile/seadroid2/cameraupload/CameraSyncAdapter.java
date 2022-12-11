package com.seafile.seadroid2.cameraupload;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.RecoverableSecurityException;
import android.content.AbstractThreadedSyncAdapter;
import android.content.ComponentName;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.ServiceConnection;
import android.content.SyncResult;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.MediaStore;
import android.support.annotation.RequiresApi;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.CameraSyncEvent;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DirentCache;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.CustomNotificationBuilder;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.ui.activity.SettingsActivity;
import com.seafile.seadroid2.util.CameraSyncStatus;
import com.seafile.seadroid2.util.Utils;

import org.greenrobot.eventbus.EventBus;

import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.text.FieldPosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Sync adapter for media upload.
 * <p/>
 * This class uploads images/videos from the gallery to the configured seafile account.
 * It is not called directly, but managed by the Android Sync Manager instead.
 */
public class CameraSyncAdapter extends AbstractThreadedSyncAdapter {
    private static final String DEBUG_TAG = "CameraSyncAdapter";
    private static final String CACHE_NAME = "CameraSync";

    private ContentResolver contentResolver;

    private SettingsManager settingsMgr = SettingsManager.instance();
    private com.seafile.seadroid2.account.AccountManager manager;
    private CameraUploadDBHelper dbHelper;

    private MediaCursor previous = null;
    private LinkedList<String> leftBuckets;
    private int synNum = 0;

    private final int leaveMeida = 2000;
    private HashMap<String, Integer> bucketMeidaNum = new HashMap<String, Integer>();

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

        if(Math.random() < 0.01){
            synNum = 30;
        }
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

    public void createDirectory(DataManager dataManager, String bucketName) throws SeafException {
        forceCreateDirectory(dataManager, BASE_DIR,bucketName);
        dataManager.getDirentsFromServer(targetRepoId, Utils.pathJoin(BASE_DIR,bucketName));
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

    @RequiresApi(api = Build.VERSION_CODES.N)
    @Override
    public void onPerformSync(android.accounts.Account account,
                              Bundle extras, String authority,
                              ContentProviderClient provider,
                              SyncResult syncResult) {

        synchronized (this) {
            cancelled = false;
        }
//        if(imageCursor != null && videoCursor != null){
//            Log.e(DEBUG_TAG, "Mixing uploading images and videos.");
//            Utils.utilsLogInfo(true,"[Error] Mixing uploading images and videos.");
//            imageCursor = null;
//            videoCursor = null;
//        }
//        if(imageCursor == null && videoCursor == null) {
//            bucketMeidaNum.clear();
//            Log.i(DEBUG_TAG, "Cleaning cache.");
//            Utils.utilsLogInfo(true,"====Cleaning cache.");
//            dbHelper.cleanRepoCache();
//            dbHelper.cleanPhotoCache();
//        }
        SeadroidApplication.getInstance().setScanUploadStatus(CameraSyncStatus.SCANNING);
        EventBus.getDefault().post(new CameraSyncEvent("start"));
        /*Log.i(DEBUG_TAG, "Syncing images and video to " + account);

        Log.d(DEBUG_TAG, "Selected buckets for camera upload: "+settingsMgr.getCameraUploadBucketList());
        Log.d(DEBUG_TAG, "is video upload allowed: "+settingsMgr.isVideosUploadAllowed());
        Log.d(DEBUG_TAG, "is data plan allowed: "+settingsMgr.isDataPlanAllowed());

        Log.d(DEBUG_TAG, "Media buckets available on this system: ");*/
//        for (GalleryBucketUtils.Bucket bucket: GalleryBucketUtils.getMediaBuckets(getContext())) {
            // Log.d(DEBUG_TAG, "Bucket id="+bucket.id+" name="+bucket.name);
//        }

        // resync all media
//        if (extras.getBoolean(ContentResolver.SYNC_EXTRAS_INITIALIZE)) {
//             Log.i(DEBUG_TAG, "Doing a full resync");
//            dbHelper.cleanPhotoCache();
//        }

        if (!settingsMgr.checkCameraUploadNetworkAvailable()) {
            // Log.d(DEBUG_TAG, "Not syncing because of data plan restriction.");
            // treat dataPlan abort the same way as a network connection error
            syncResult.stats.numIoExceptions++;
            SeadroidApplication.getInstance().setScanUploadStatus(CameraSyncStatus.NETWORK_UNAVAILABLE);
            EventBus.getDefault().post(new CameraSyncEvent("noNetwork"));
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
            int timeout = 10000; // wait up to a second
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
            createDirectories(dataManager);
            uploadBuckets(syncResult, dataManager);

            if (isCancelled()) {
                 Log.i(DEBUG_TAG, "sync was cancelled.");
            } else {
                 Log.i(DEBUG_TAG, "sync finished successfully.");
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
            Utils.utilsLogInfo(true, "sync aborted because an unknown error: " + e.getMessage());
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
        SeadroidApplication.getInstance().setScanUploadStatus(CameraSyncStatus.SCAN_END);
        SettingsManager.instance().saveUploadCompletedTime(Utils.getSyncCompletedTime());
        EventBus.getDefault().post(new CameraSyncEvent("end"));
    }

    private void uploadBuckets(SyncResult syncResult, DataManager dataManager) throws SeafException, InterruptedException{
        Utils.utilsLogInfo(true, "========Starting to upload buckets...");
        if((leftBuckets == null || leftBuckets.size() == 0) && previous == null){
            if(synNum > 30) {
                Utils.utilsLogInfo(true, "========Clear photo cache...");
                dbHelper.cleanPhotoCache();
                synNum = 0;
            }else {
                ++synNum;
            }
        }

        if (isCancelled()){
            Utils.utilsLogInfo(true, "========Cancel uploading========");
            return;
        }

        List<String> selectedBuckets = new ArrayList<>();
        if(leftBuckets != null && leftBuckets.size() > 0){
            selectedBuckets = leftBuckets;
        }else if (bucketList != null && bucketList.size() > 0) {
            selectedBuckets = bucketList;
        }

        Utils.utilsLogInfo(true, "========Traversal all phone buckets========");
        List<GalleryBucketUtils.Bucket> allBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
        Map<String, String> bucketNames = new HashMap<String, String>();
        for (GalleryBucketUtils.Bucket bucket : allBuckets) {
            if (bucket.isCameraBucket && bucketList.size() == 0) {
                selectedBuckets.add(bucket.id);
            }
            bucketNames.put(bucket.id, bucket.name);
        }
        if(previous != null){
            Utils.utilsLogInfo(true, "========Continue uploading previous cursor========");
            previous = iterateCursor(syncResult, dataManager, previous);
            if(isCancelled()){
                Utils.utilsLogInfo(true, "========Cancel uploading========");
                return;
            }
        }

        Utils.utilsLogInfo(true, "========Copy select buckets========");
        leftBuckets = Lists.newLinkedList();
        leftBuckets.addAll(selectedBuckets);

        Utils.utilsLogInfo(true, "========Start traversal selected buckets========");
        for(String bucketID: selectedBuckets) {
            leftBuckets.removeFirst();
            String bucketName = bucketNames.get(bucketID);
            if(bucketName == null || bucketName.length() == 0){
                Utils.utilsLogInfo(true, "========Bucket "+ bucketName + " does not exist.");
                continue;
            }else{
                Utils.utilsLogInfo(true, "========Uploading "+ bucketName+"...");
            }
            String[] selectionArgs = new String[]{bucketID};
            String selection = MediaStore.Images.ImageColumns.BUCKET_ID + " = ? ";
            Cursor imageCursor = contentResolver.query(
                    MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                    new String[]{
                            MediaStore.Images.Media._ID,
                            MediaStore.Images.Media.DISPLAY_NAME,
                            MediaStore.Images.Media.DATE_MODIFIED,
                            MediaStore.Images.Media.SIZE,
                            MediaStore.Images.Media.DATA,
                            MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME
                    },
                    selection,
                    selectionArgs,
                    MediaStore.Images.ImageColumns.DISPLAY_NAME + " ASC"
            );
            Cursor videoCursor = null;
            if(settingsMgr.isVideosUploadAllowed()){
                selectionArgs = new String[]{bucketID};
                selection = MediaStore.Video.VideoColumns.BUCKET_ID + " = ? ";
                videoCursor = contentResolver.query(
                        MediaStore.Video.Media.EXTERNAL_CONTENT_URI,
                        new String[]{
                                MediaStore.Video.Media._ID,
                                MediaStore.Video.Media.DISPLAY_NAME,
                                MediaStore.Video.Media.DATE_MODIFIED,
                                MediaStore.Video.Media.SIZE,
                                MediaStore.Video.Media.DATA,
                                MediaStore.Video.VideoColumns.BUCKET_DISPLAY_NAME
                        },
                        selection,
                        selectionArgs,
                        MediaStore.Video.VideoColumns.DISPLAY_NAME + " ASC"
                );
            }
            MediaCursor cursor = new MediaCursor(bucketName, imageCursor, videoCursor);
            if(cursor.getCount() > 0){
                createDirectory(dataManager, bucketName);
                if (cursor.getFilePath().startsWith(StorageManager.getInstance().getMediaDir().getAbsolutePath())) {
                    Log.d(DEBUG_TAG, "Skipping media "+ bucketName +" because it's part of the Seadroid cache");
                }else {
                    previous = iterateCursor(syncResult, dataManager, cursor);
                }
            }else {
                previous = null;
            }
            if(isCancelled()){
                Utils.utilsLogInfo(true, "========Cancel uploading========");
                break;
            }
        }
    }

    private MediaCursor iterateCursor(SyncResult syncResult, DataManager dataManager, MediaCursor cursor) throws SeafException, InterruptedException{
        if(cursor == null || cursor.getCount() == 0){
            Utils.utilsLogInfo(true,"=======Empty Cursor.===");
            return null;
        }
        String bucketName = cursor.getBucketName();
        int fileIter = cursor.getPosition();
        int fileNum = cursor.getCount();

        Utils.utilsLogInfo(true, "========Found ["+ fileIter+"/"+fileNum+"] images in bucket "+bucketName+"========");
        try {
            DirentCache cache = getCache(targetRepoId, bucketName, dataManager);
            if(cache != null) {
                int n = cache.getCount();
                boolean done = false;
                for (int i = 0; i < n; ++i) {
                    if (cursor.isAfterLast()) {
                        break;
                    }
                    SeafDirent item = cache.get(i);
                    while (cursor.getFileName().compareTo(item.name) <= 0) {
                        if (cursor.getFileName().compareTo(item.name) < 0 || item.size < cursor.getFileSize() && item.mtime < cursor.getFileModified()) {
                            File file = cursor.getFile();
                            if (file != null && file.exists() && dbHelper.isUploaded(file.getPath(), file.lastModified())) {
                                Log.d(DEBUG_TAG, "Skipping media " + file.getPath() + " because we have uploaded it in the past.");
                            } else {
                                if (file == null || !file.exists()) {
                                    Log.d(DEBUG_TAG, "Skipping media " + file + " because it doesn't exist");
                                    syncResult.stats.numSkippedEntries++;
                                } else {
                                    uploadFile(dataManager, file, bucketName);
                                }
                            }
                        } else {
//                        ++uploadedNum;
//                        if(uploadedNum > leaveMeida){
//                            if(cursor.deleteFile()){
//                                Utils.utilsLogInfo(true, "====File " + cursor.getFileName() + " in bucket " + bucketName + " is deleted because it exists on the server. Skipping.");
//                            }
//                        }
                            Log.d(DEBUG_TAG, "====File " + cursor.getFilePath() + " in bucket " + bucketName + " already exists on the server. Skipping.");
                            dbHelper.markAsUploaded(cursor.getFilePath(), cursor.getFileModified());
                        }
                        ++fileIter;
                        if (!cursor.moveToNext()) {
                            break;
                        }
                    }
                    if (i == n - 1) {
                        done = true;
                    }
                    if (isCancelled()) {
                        break;
                    }

                }
                if(done){
                    cache.delete();
                }
            }
            while (!cursor.isAfterLast() && !isCancelled()) {
                File file = cursor.getFile();
                uploadFile(dataManager, file, bucketName);
                ++fileIter;
                if(!cursor.moveToNext()){
                    break;
                }
            }
        }catch (IOException e){
            Log.e(DEBUG_TAG, "Failed to get cache file.", e);
            Utils.utilsLogInfo(true, "Failed to get cache file: "+ e.toString());
        }catch (Exception e){
            Utils.utilsLogInfo(true, e.toString());
        }
        Utils.utilsLogInfo(true,"=======Have checked ["+Integer.toString(fileIter)+"/"+Integer.toString(fileNum)+"] images in "+bucketName+".===");
        Utils.utilsLogInfo(true,"=======waitForUploads===");
        waitForUploads();
        checkUploadResult(syncResult);
        if(cursor.isAfterLast()){
            return null;
        }
        return cursor;
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
//                Utils.utilsLogInfo(true,"=======Task "+ Integer.toString(id) + " fail to uploaded!!!");
//                throw info.err;
                continue;
            }
            if (info.state == TaskState.FINISHED) {
                File file = new File(info.localFilePath);
                dbHelper.markAsUploaded(file);
                syncResult.stats.numInserts++;
            } else {
//                Utils.utilsLogInfo(true,"=======Task "+ Integer.toString(id) + " has an unknown exception!!!");
//                throw SeafException.unknownException;
                continue;
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
        Log.d(DEBUG_TAG, "uploading file " + file.getName() + " to " + serverPath);
        Utils.utilsLogInfo(true,"====uploading file " + file.getName() + " to " + serverPath);
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

    protected class MediaCursor{
        private Cursor imageCursor;
        private Cursor videoCursor;
        private String bucketName;
        private Cursor p;
        private Comparator<Cursor> comp;

        protected class DefaultCompartor implements Comparator<Cursor> {
            @Override
            public int compare(Cursor o1, Cursor o2) {
                if(o1 == null || o1.isAfterLast()){
                    return 1;
                }
                if(o2 == null || o2.isAfterLast()){
                    return -1;
                }
                return o1.getString(o1.getColumnIndex(MediaStore.Images.Media.DISPLAY_NAME)).compareTo(o2.getString(o2.getColumnIndex(MediaStore.Video.Media.DISPLAY_NAME)));
            }
        };

        public MediaCursor(String bucketName, Cursor imageCursor, Cursor videoCursor){
            init(bucketName, imageCursor, videoCursor,  new DefaultCompartor());
        }

        public MediaCursor(String bucketName, Cursor imageCursor, Cursor videoCursor, Comparator<Cursor> comp){
            if(comp == null){
                comp = new DefaultCompartor();
            }
            init(bucketName, imageCursor, videoCursor, comp);
        }

        private Cursor initializeCursor(Cursor cursor){
            if(cursor != null && cursor.isBeforeFirst()){
                cursor.moveToNext();
            }
            if(cursor == null || cursor.isAfterLast()){
                if(cursor != null){
                    cursor.close();
                }
                return null;
            }
            return cursor;
        }

        private void init(String bucketName, Cursor imageCursor, Cursor videoCursor, Comparator<Cursor> comp){
            this.bucketName = bucketName;
            imageCursor = initializeCursor(imageCursor);
            videoCursor = initializeCursor(videoCursor);
            this.imageCursor = imageCursor;
            this.videoCursor = videoCursor;
            if(this.imageCursor == null && this.videoCursor == null){
                this.p = null;
            }else if(this.imageCursor == null){
                this.p = videoCursor;
            }else if(this.videoCursor == null){
                this.p = imageCursor;
            }else{
                if(comp.compare(imageCursor, videoCursor) <= 0){
                    this.p = imageCursor;
                }else{
                    this.p = videoCursor;
                }
            }
            this.comp = comp;
        }


        public int getCount(){
            if(imageCursor == null && videoCursor == null){
                return 0;
            }else if(imageCursor == null){
                return videoCursor.getCount();
            }else if(videoCursor == null){
                return imageCursor.getCount();
            }else{
                return imageCursor.getCount() + videoCursor.getCount();
            }
        }

        public int getPosition(){
            if(imageCursor == null && videoCursor == null){
                return 0;
            }else if(imageCursor == null){
                return videoCursor.getPosition();
            }else if(videoCursor == null){
                return imageCursor.getPosition();
            }else{
                return imageCursor.getPosition() + videoCursor.getPosition();
            }
        }

        public boolean isAfterLast(){
            return (this.imageCursor == null || this.imageCursor.isAfterLast())
                    && (this.videoCursor == null || this.videoCursor.isAfterLast());
        }

        public boolean moveToNext(){
            if(isAfterLast() || p == null){
                return false;
            }
            boolean res = p.moveToNext();
            if(comp.compare(imageCursor, videoCursor) <= 0){
                p = imageCursor;
            }else{
                p = videoCursor;
            }
            return res;
        }

        public String getFileName(){
            if(p == null){
                return "";
            }
            if(p == imageCursor){
                return p.getString(p.getColumnIndex(MediaStore.Images.Media.DISPLAY_NAME));
            }else{
                return p.getString(p.getColumnIndex(MediaStore.Video.Media.DISPLAY_NAME));
            }
        }

        public int getFileModified(){
            if(p == null){
                return -1;
            }
            if(p == imageCursor){
                return p.getInt(p.getColumnIndex(MediaStore.Images.Media.DATE_MODIFIED));
            }else{
                return p.getInt(p.getColumnIndex(MediaStore.Video.Media.DATE_MODIFIED));
            }
        }

        public File getFile(){
            String path = getFilePath();
            if(path == null || path.length() == 0){
                return null;
            }
            return new File(getFilePath());
        }

        public Uri getFileUri(){
            if(p == null){
                return Uri.EMPTY;
            }
            if(p == imageCursor){
                String id = p.getString(p.getColumnIndex(MediaStore.Images.Media._ID));
                return Uri.withAppendedPath(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, id);
            }else{
                String id = p.getString(p.getColumnIndex(MediaStore.Video.Media._ID));
                return Uri.withAppendedPath(MediaStore.Video.Media.EXTERNAL_CONTENT_URI, id);
            }
        }

        public String getFileID(){
            if(p == null){
                return "";
            }
            if(p == imageCursor){
                return p.getString(p.getColumnIndex(MediaStore.Images.Media._ID));
            }else{
                return p.getString(p.getColumnIndex(MediaStore.Video.Media._ID));
            }
        }

        public String getFilePath(){
            if(p == null){
                return "";
            }
            if(p == imageCursor){
                String id = p.getString(p.getColumnIndex(MediaStore.Images.Media._ID));
                Uri uri = Uri.withAppendedPath(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, id);
                if(uri == null){
                    return null;
                }
                return Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), uri, "images");
            }else{
                String id = p.getString(p.getColumnIndex(MediaStore.Video.Media._ID));
                Uri uri = Uri.withAppendedPath(MediaStore.Video.Media.EXTERNAL_CONTENT_URI, id);
                if(uri == null){
                    return null;
                }
                return Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), uri, "video");
            }
        }

        public int getFileSize(){
            if(p == null){
                return -1;
            }
            if(p == imageCursor){
                return p.getInt(p.getColumnIndex(MediaStore.Images.Media.SIZE));
            }else{
                return p.getInt(p.getColumnIndex(MediaStore.Video.Media.SIZE));
            }
        }

        public String getBucketName(){
            if(p == null){
                return "";
            }
            if(p.isBeforeFirst()){
                return bucketName;
            }
            if(p == imageCursor){
                return p.getString(p.getColumnIndex(MediaStore.Images.Media.BUCKET_DISPLAY_NAME));
            }else{
                return p.getString(p.getColumnIndex(MediaStore.Video.Media.BUCKET_DISPLAY_NAME));
            }
        }

        public boolean deleteFile(){
            File file = getFile();
            if(file == null || !file.exists()){
                return false;
            }
            return file.delete();
        }

        @Override
        protected void finalize() throws Throwable {
            super.finalize();
            if(imageCursor!=null) {
                imageCursor.close();
            }
            if(videoCursor!=null) {
                videoCursor.close();
            }
        }
    }

    private DirentCache getCache(String repoID, String bucketName, DataManager dataManager) throws IOException, InterruptedException, SeafException{
        String name = CACHE_NAME+repoID+"-"+bucketName;
        String serverPath = Utils.pathJoin(BASE_DIR, bucketName);
//        if(DirentCache.cacheFileExists(name)){
//            return new DirentCache(name);
//        }
        Utils.utilsLogInfo(true, "=======savingRepo===");
        List<SeafDirent> seafDirents = dataManager.getCachedDirents(repoID, serverPath);
        int timeOut = 10000; // wait up to a second
        while (seafDirents == null && timeOut > 0) {
            // Log.d(DEBUG_TAG, "waiting for transfer service");
            Thread.sleep(100);
            seafDirents = dataManager.getDirentsFromServer(targetRepoId, serverPath);
            timeOut -= 100;
        }
        if (seafDirents == null) {
            return null;
        }
        return new DirentCache(name, seafDirents, new Comparator<SeafDirent>() {
            @Override
            public int compare(SeafDirent o1, SeafDirent o2) {
                return o1.name.compareTo(o2.name);
            }
        });
    }
}


