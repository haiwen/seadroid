package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;
import android.database.Cursor;
import android.provider.MediaStore;
import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.google.common.base.Joiner;
import com.google.common.base.Stopwatch;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.notification.TransferNotificationDispatcher;
import com.seafile.seadroid2.framework.service.ParentEventTransfer;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import okhttp3.RequestBody;
import retrofit2.Call;

public class MediaBackupScanner extends ParentEventTransfer {
    private final String TAG = "MediaBackupScanner";
    public static final String BASE_DIR = "My Photos";

    public MediaBackupScanner(Context context) {
        super(context);
    }

    protected SeafException returnSuccess() {
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
        return SeafException.SUCCESS;
    }

    public SeafException scan(boolean isForce) {
        SafeLogs.d(TAG, "doWork()", "started execution");
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the switch is off");
            return returnSuccess();
        }

        Account backupAccount = CameraUploadManager.getInstance().getCameraAccount();
        if (backupAccount == null) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the backup account is null");
            return returnSuccess();
        }

        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || TextUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "doWork()", "the album scan task was not started, because the repoConfig is null");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        if (isForce) {
            AlbumBackupSharePreferenceHelper.resetLastScanTime();
        }

        SafeLogs.d(TAG, "doWork()", "start scan");
        send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCANNING);

        SeafException seafException = loadMedia(account, repoConfig);
        if (seafException != SeafException.SUCCESS) {
            SafeLogs.d(TAG, "doWork()", "loadMedia() failed：" + seafException.getMessage());
            send(FeatureDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
            return seafException;
        }

        //
        int totalPendingCount = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getPendingCount();
        String content = null;
        if (totalPendingCount > 0) {
            boolean isAllowUpload = checkNetworkTypeIfAllowStartUploadWorker();
            if (!isAllowUpload) {
                content = TransferResult.WAITING.name();
            }
        }

        //
        sendCompleteEvent(FeatureDataSource.ALBUM_BACKUP, content, totalPendingCount);

        return SeafException.SUCCESS;
    }

    protected boolean checkNetworkTypeIfAllowStartUploadWorker() {
        boolean isAllowData = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (isAllowData) {
            return true;
        }

        return !NetworkUtils.isMobileData();
    }

    private SeafException loadMedia(Account account, RepoConfig repoConfig) {
        // check
        List<String> bucketIdList = AlbumBackupSharePreferenceHelper.readBucketIds();

        if (CollectionUtils.isEmpty(bucketIdList)) {
            List<GalleryBucketUtils.Bucket> allBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
            if (allBuckets == null) {
                SafeLogs.d(TAG, "doWork()", "no media in local storage, may be has no permission");
                return SeafException.SUCCESS;
            }

            for (GalleryBucketUtils.Bucket bucket : allBuckets) {
                //if user choose to back up the default album, only the "Camera" folder on phone will be read
                if (bucket.isCameraBucket) {
                    bucketIdList.add(bucket.bucketId);
                }
            }
        }

        Stopwatch stopwatch = Stopwatch.createStarted();

        long lastScanTime = AlbumBackupSharePreferenceHelper.readLastScanTimeMills();

        //query images
        loadImages(lastScanTime, account, repoConfig, bucketIdList);

        //query videos
        if (AlbumBackupSharePreferenceHelper.readAllowVideoSwitch()) {
            loadVideos(lastScanTime, account, repoConfig, bucketIdList);
        }

        stopwatch.stop();

        long diff = stopwatch.elapsed(TimeUnit.MILLISECONDS);
        SafeLogs.d(TAG, "doWork()", "album backup scan time：" + stopwatch);
        long now = System.currentTimeMillis();
        AlbumBackupSharePreferenceHelper.writeLastScanTime(now - diff);

        // create directories for media buckets
        try {
            createDirectories(repoConfig.getRepoId());
            return SeafException.SUCCESS;
        } catch (SeafException seafException) {
            return seafException;
        }
    }

    /**
     * If lastScanTimeLong is not 0, the time range from the previous 5 minutes of lastScanTimeLong to the now( >= ?)
     *
     * @param lastScanTimeLong mills
     */
    private void loadImages(long lastScanTimeLong, Account account, RepoConfig repoConfig, List<String> bucketIdList) {
        if (bucketIdList.isEmpty()) {
            SafeLogs.d(TAG, "loadImages()", "no media in local storage");
            return;
        }

        String[] selectionArgs = bucketIdList.toArray(new String[]{});
        String selection = MediaStore.Images.ImageColumns.BUCKET_ID + " IN " + varArgs(bucketIdList.size());

        //incremental queries
        if (lastScanTimeLong > 0) {
            //query
            selection += " and " + MediaStore.Images.Media.DATE_ADDED + " >= ? "; // If it's >=, might get some duplicate data
            selectionArgs = Arrays.copyOf(selectionArgs, selectionArgs.length + 1);
            selectionArgs[selectionArgs.length - 1] = String.valueOf(lastScanTimeLong / 1000);
        }

        Cursor cursor = getContext().getContentResolver().query(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                null,
                selection,
                selectionArgs,
                MediaStore.Images.ImageColumns.DATE_ADDED + " DESC"
        );

        try {
            if (cursor == null) {
                return;
            }

            SafeLogs.d(TAG, "loadImages()", "images query count : " + cursor.getCount());
            if (cursor.getCount() == 0) {
                return;
            }

            //iterate
            iterateCursor(cursor, account, repoConfig);
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
    }

    private void loadVideos(long lastScanTimeLong, Account account, RepoConfig repoConfig, List<String> bucketIdList) {

        if (bucketIdList.isEmpty()) {
            SafeLogs.d(TAG, "loadVideos()", "no media in local storage");
            return;
        }

        String[] selectionArgs = bucketIdList.toArray(new String[]{});
        String selection = MediaStore.Video.VideoColumns.BUCKET_ID + " IN " + varArgs(bucketIdList.size());

        if (lastScanTimeLong > 0) {
            //query
            selection += " and " + MediaStore.Images.Media.DATE_ADDED + " >= ? "; // If it's >=, might get some duplicate data
            selectionArgs = Arrays.copyOf(selectionArgs, selectionArgs.length + 1);
            selectionArgs[selectionArgs.length - 1] = String.valueOf(lastScanTimeLong / 1000);
        }

        Cursor cursor = getContext().getContentResolver().query(
                MediaStore.Video.Media.EXTERNAL_CONTENT_URI,
                null,
                selection,
                selectionArgs,
                MediaStore.Video.VideoColumns.DATE_ADDED + " DESC"
        );

        try {
            if (cursor == null) {
                return;
            }
            SafeLogs.d(TAG, "loadVideos()", "video query count : " + cursor.getCount());
            if (cursor.getCount() == 0) {
                return;
            }

            //
            iterateCursor(cursor, account, repoConfig);
        } finally {
            if (cursor != null) {
                cursor.close();
            }
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
     * @param cursor
     */
    private void iterateCursor(Cursor cursor, Account account, RepoConfig repoConfig) {
        String localCacheAbsPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();

        // load them one by one
        while (cursor.moveToNext()) {

            int bucketColumn = cursor.getColumnIndexOrThrow(MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME);
            String bucketName = cursor.getString(bucketColumn);

            int dateAddIndex = cursor.getColumnIndex(MediaStore.Images.Media.DATE_ADDED);
            long dateAdded = cursor.getLong(dateAddIndex);
            String dateAddedString = TimeUtils.millis2String(dateAdded * 1000, "yyyy-MM-dd HH:mm:ss");

            int dataIndex = cursor.getColumnIndex(MediaStore.Images.Media.DATA);
            String localPath = cursor.getString(dataIndex);
            if (TextUtils.isEmpty(localPath)) {
                SafeLogs.d(TAG, "iterateCursor()", "skip file -> [localPath is null] dataIndex: " + dataIndex + ", because it doesn't exist");
                continue;
            }

//            String p = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), videoUri, media);

            File file = new File(localPath);
            if (!file.exists()) {
                // local file does not exist. some inconsistency in the Media Provider? Ignore and continue
                SafeLogs.d(TAG, "iterateCursor()", "skip file -> [not exists] " + localPath + ", because it doesn't exist");
                continue;
            }

            // Ignore all media by Seafile. We don't want to upload our own cached files.
            if (file.getAbsolutePath().startsWith(localCacheAbsPath)) {
                SafeLogs.d(TAG, "iterateCursor()", "skip file -> [cache file] " + localPath + ", because it's part of the Seadroid cache");
                continue;
            }

            List<FileBackupStatusEntity> transferEntityList = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getListByFullPathSync(repoConfig.getRepoId(), TransferDataSource.ALBUM_BACKUP, file.getAbsolutePath());

            if (!CollectionUtils.isEmpty(transferEntityList)) {
                SafeLogs.d(TAG, "iterateCursor()", "skip file -> [local exists] " + localPath + ", because we have uploaded it in the past.");
                continue;
            }

            SafeLogs.d(TAG, "iterateCursor()", "new file -> [wait for check] " + localPath);

            //cache
            TransferModel transferModel = TransferModel.convert(file, bucketName, 0); // 0 is just a symbol, no means
            transferModel.related_account = account.getSignature();
            transferModel.repo_id = repoConfig.getRepoId();
            transferModel.repo_name = repoConfig.getRepoName();

            transferModel.data_source = FeatureDataSource.ALBUM_BACKUP;
            transferModel.save_to = SaveTo.DB;
            transferModel.setId(transferModel.genStableId());
            GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.put(bucketName, transferModel);
        }
    }

    /**
     * Create all the subdirectories on the server for the buckets that are about to be uploaded.
     */
    private void createDirectories(String repoId) throws SeafException {

        Set<String> keys = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getCategoryKeys();

        // create base directory
        createBucketDirectoryIfNecessary(repoId, "/", BASE_DIR);

        for (String bucketName : keys) {
            createBucketDirectoryIfNecessary(repoId, "/" + BASE_DIR, bucketName);
        }
    }

    /**
     * Create a directory, rename a file away if necessary,
     *
     * @param bucketName directory to create
     */
    private void createBucketDirectoryIfNecessary(String repoId, String remoteParentPath, String bucketName) throws SeafException {
        remoteParentPath = Utils.pathJoin(remoteParentPath, "/");// / -> /

        DirentWrapperModel direntWrapperModel = getDirentWrapper(repoId, remoteParentPath);

        String curPath = Utils.pathJoin(remoteParentPath, bucketName);
        boolean found = false;

        //check whether the file in the parent directory contains the bucket name
        List<DirentModel> parentDirentList = direntWrapperModel.dirent_list;
        for (DirentModel dirent : parentDirentList) {
            if (!TextUtils.equals(dirent.name, bucketName)) {
                continue;
            }

            //same folder name
            if (dirent.isDir()) {
                found = true;
            } else {
                //if a file same with the bucketName, rename it
                renameRemoteFile(repoId, dirent.name, curPath);
            }

            break;
        }

        //if not, create bucketName dir.
        if (!found) {
            mkdirRemote(repoId, curPath);
        }

        //
        checkAndInsert(repoId, remoteParentPath, bucketName);
    }

    private void checkAndInsert(String repoId, String remoteParentPath, String bucketName) throws SeafException {
        String path = Utils.pathJoin(remoteParentPath, bucketName);

        List<TransferModel> pendingList = null;
        if (!TextUtils.equals("/", remoteParentPath)) {
            pendingList = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getCategoryTransferList(bucketName);
        }

        if (CollectionUtils.isEmpty(pendingList)) {
            return;
        }

        DirentWrapperModel direntWrapperModel = getDirentWrapper(repoId, path);

        List<DirentModel> remoteList = direntWrapperModel.dirent_list;
        if (CollectionUtils.isEmpty(remoteList)) {
            return;
        }

        for (TransferModel transferModel : pendingList) {
            String filename = transferModel.getFileName();

            String prefix, suffix;
            if (filename.contains(".")) {
                prefix = filename.substring(0, filename.lastIndexOf("."));
                suffix = filename.substring(filename.lastIndexOf("."));
            } else {
                prefix = filename;
                suffix = "";
            }
            Pattern pattern = Pattern.compile(Pattern.quote(prefix) + "( \\(\\d+\\))?" + Pattern.quote(suffix));
            /*
             * It would be cool if the API2 offered a way to query the hash of a remote file.
             * Currently, comparing the file size is the best we can do.
             */
            Optional<DirentModel> firstOp = remoteList.stream()
                    .filter(f -> pattern.matcher(f.name).matches())
                    .findFirst();

            if (firstOp.isPresent()) {
                SafeLogs.d(TAG, "checkAndInsert()", "skip file -> [remote exists] " + filename + ", because we have uploaded it in the past.");

                GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.remove(bucketName, transferModel);
            }
        }
    }

    private DirentWrapperModel getDirentWrapper(String repoId, String parentPath) throws SeafException {
        //get parent dirent list from remote
        retrofit2.Response<DirentWrapperModel> res;
        try {
            Call<DirentWrapperModel> call = HttpIO.getCurrentInstance()
                    .execute(RepoService.class)
                    .getDirentsSync(repoId, parentPath);
            res = call.execute();
        } catch (IOException e) {
            SafeLogs.d(TAG, "getDirentWrapper()", "request dirents failed", e.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (!res.isSuccessful()) {
            SafeLogs.d(TAG, "getDirentWrapper()", "request dirents failed");
            throw SeafException.NETWORK_EXCEPTION;
        }

        DirentWrapperModel tempWrapper = res.body();
        if (tempWrapper == null) {
            SafeLogs.d(TAG, "getDirentWrapper()", "request dirents is null");
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (!TextUtils.isEmpty(tempWrapper.error_msg)) {
            SafeLogs.d(TAG, "getDirentWrapper()", tempWrapper.error_msg);
            throw SeafException.NETWORK_EXCEPTION;
        }
        return tempWrapper;
    }


    protected void mkdirRemote(String repoId, String path) throws SeafException {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "mkdir");
        requestDataMap.put("create_parents", "true");

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        retrofit2.Response<String> res;

        try {
            Call<String> call = HttpIO.getCurrentInstance()
                    .execute(FileService.class)
                    .mkDirCall(repoId, path, requestBodyMap);
            res = call.execute();
        } catch (IOException e) {
            SafeLogs.d(TAG, "mkdirRemote()", "mkdir failed", e.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (res.isSuccessful()) {
            SafeLogs.d(TAG, "mkdirRemote()", "folder create success: " + path);
        } else {
            SafeLogs.d(TAG, "mkdirRemote()", "folder create failed: " + path);
        }
    }

    private void renameRemoteFile(String repoId, String name, String fullPath) throws SeafException {
        // there is already a file. move it away.
        String newFilename = getContext().getString(R.string.camera_sync_rename_file, name);

        Map<String, String> renameMap = new HashMap<>();
        renameMap.put("operation", "rename");
        renameMap.put("newname", newFilename);
        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(renameMap);

        retrofit2.Response<String> res;

        try {
            Call<String> call = HttpIO.getCurrentInstance()
                    .execute(FileService.class)
                    .renameFileCall(repoId, fullPath, requestBodyMap);
            res = call.execute();
        } catch (IOException e) {
            SafeLogs.d(TAG, "renameRemoteFile()", "renameRemoteFile failed", e.getMessage());
            throw SeafException.NETWORK_EXCEPTION;
        }

        if (res.isSuccessful()) {
            SafeLogs.d(TAG, "renameRemoteFile()", "Folder rename success：" + fullPath);
        } else {
            SafeLogs.d(TAG, "renameRemoteFile()", "Folder rename failed：" + fullPath);
        }
    }
}
