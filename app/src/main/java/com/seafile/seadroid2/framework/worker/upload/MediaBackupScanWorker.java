package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.database.Cursor;
import android.os.Build;
import android.provider.MediaStore;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.google.common.base.Joiner;
import com.google.common.base.Stopwatch;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
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
import com.seafile.seadroid2.framework.notification.AlbumBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
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
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import okhttp3.RequestBody;
import retrofit2.Call;

/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class MediaBackupScanWorker extends BaseScanWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(MediaBackupScanWorker.class.getSimpleName().getBytes());

    private final AlbumBackupScanNotificationHelper notificationManager;

    public static final String BASE_DIR = "My Photos";

    private RepoConfig repoConfig;
    private Account account;

    /**
     * key: bucketName
     */
    private List<String> bucketIdList;

    public MediaBackupScanWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new AlbumBackupScanNotificationHelper(context);

        account = SupportAccountManager.getInstance().getCurrentAccount();
    }


    private void showNotification() {

        String title = getApplicationContext().getString(R.string.settings_camera_upload_info_title);
        String subTitle = getApplicationContext().getString(R.string.is_scanning);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title, subTitle);
                showForegroundAsync(foregroundInfo);
            } catch (ForegroundServiceStartNotAllowedException e) {
                SLogs.d(e.getMessage());
            }
        } else {
            ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification(title, subTitle);
            showForegroundAsync(foregroundInfo);
        }
    }

    @Override
    public TransferDataSource getDataSource() {
        return TransferDataSource.ALBUM_BACKUP;
    }

    @NonNull
    @Override
    public Result doWork() {
        account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            SLogs.d("the album scan task was not started, because the switch is off");
            return returnSuccess();
        }

        Account backupAccount = CameraUploadManager.getInstance().getCameraAccount();
        if (backupAccount == null) {
            SLogs.d("the album scan task was not started, because the backup account is null");
            return returnSuccess();
        }

        repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || TextUtils.isEmpty(repoConfig.getRepoId())) {
            SLogs.d("the album scan task was not started, because the repoConfig is null");
            return returnSuccess();
        }

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            AlbumBackupSharePreferenceHelper.resetLastScanTime();
        }

        // check
        bucketIdList = AlbumBackupSharePreferenceHelper.readBucketIds();

        showNotification();

        try {
            SLogs.d("MediaSyncWorker start");
            sendWorkerEvent(TransferDataSource.ALBUM_BACKUP, TransferEvent.EVENT_SCANNING);

            loadMedia();

        } catch (SeafException | IOException e) {
            SLogs.d("MediaBackupScannerWorker has occurred error", e);
        } finally {
            AlbumBackupSharePreferenceHelper.writeLastScanTime(System.currentTimeMillis());
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
        sendFinishScanEvent(content, totalPendingCount);
        return Result.success();
    }


    private void loadMedia() throws SeafException, IOException {
        if (CollectionUtils.isEmpty(bucketIdList)) {
            List<GalleryBucketUtils.Bucket> allBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
            if (allBuckets == null) {
                SLogs.d("no media in local storage, may be has no permission");
                return;
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
        loadImages(lastScanTime);

        if (AlbumBackupSharePreferenceHelper.readAllowVideoSwitch()) {
            loadVideos(lastScanTime);
        }

        stopwatch.stop();
        long diff = stopwatch.elapsed(TimeUnit.MILLISECONDS);
        SLogs.d("album backup scan time：" + stopwatch);
        long now = System.currentTimeMillis();
        AlbumBackupSharePreferenceHelper.writeLastScanTime(now - diff);

        // create directories for media buckets
        createDirectories();
    }

    /**
     * If lastScanTimeLong is not 0, the time range from the previous 5 minutes of lastScanTimeLong to the now( >= ?)
     *
     * @param lastScanTimeLong mills
     */
    private void loadImages(long lastScanTimeLong) {
        if (isStopped())
            return;

        if (bucketIdList.isEmpty()) {
            SLogs.d("no media in local storage");
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

        Cursor cursor = getApplicationContext().getContentResolver().query(
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

            SLogs.d("images query count : " + cursor.getCount());
            if (cursor.getCount() == 0) {
                return;
            }

            //iterate
            iterateCursor(cursor);
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
    }

    private void loadVideos(long lastScanTimeLong) {
        if (isStopped())
            return;

        if (bucketIdList.isEmpty()) {
            SLogs.d("no media in local storage");
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

        Cursor cursor = getApplicationContext().getContentResolver().query(
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

            if (cursor.getCount() == 0) {
                return;
            }

            //
            iterateCursor(cursor);
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
    private void iterateCursor(Cursor cursor) {
        String localCacheAbsPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();

        // load them one by one
        while (!isStopped() && cursor.moveToNext()) {

            int bucketColumn = cursor.getColumnIndexOrThrow(MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME);
            String bucketName = cursor.getString(bucketColumn);

            int dateAddIndex = cursor.getColumnIndex(MediaStore.Images.Media.DATE_ADDED);
            long dateAdded = cursor.getLong(dateAddIndex);
            String dateAddedString = TimeUtils.millis2String(dateAdded * 1000, "yyyy-MM-dd HH:mm:ss");

            int dataIndex = cursor.getColumnIndex(MediaStore.Images.Media.DATA);
            String localPath = cursor.getString(dataIndex);
            if (TextUtils.isEmpty(localPath)) {
                SLogs.i("skip file -> [localPath is null] dataIndex: " + dataIndex + ", because it doesn't exist");
                continue;
            }

//            String p = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), videoUri, media);

            File file = new File(localPath);
            if (!file.exists()) {
                // local file does not exist. some inconsistency in the Media Provider? Ignore and continue
                SLogs.i("skip file -> [not exists] " + localPath + ", because it doesn't exist");
                continue;
            }

            // Ignore all media by Seafile. We don't want to upload our own cached files.
            if (file.getAbsolutePath().startsWith(localCacheAbsPath)) {
                SLogs.i("skip file -> [cache file] " + localPath + ", because it's part of the Seadroid cache");
                continue;
            }

            List<FileBackupStatusEntity> transferEntityList = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .getListByFullPathSync(repoConfig.getRepoId(), TransferDataSource.ALBUM_BACKUP, file.getAbsolutePath());

            if (!CollectionUtils.isEmpty(transferEntityList)) {
                SLogs.d("skip file -> [local exists] " + localPath + ", because we have uploaded it in the past.");
                continue;
            }

            SLogs.d("new file -> [wait for check] " + localPath);

            //cache
            TransferModel transferModel = TransferModel.convert(file, bucketName, 0); // 0 is just a symbol, no means
            transferModel.related_account = account.getSignature();
            transferModel.repo_id = repoConfig.getRepoId();
            transferModel.repo_name = repoConfig.getRepoName();

            transferModel.data_source = TransferDataSource.ALBUM_BACKUP;
            transferModel.save_to = SaveTo.DB;
            transferModel.setId(transferModel.genStableId());
            GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.put(bucketName, transferModel);
        }
    }

    /**
     * Create all the subdirectories on the server for the buckets that are about to be uploaded.
     */
    private void createDirectories() throws SeafException, IOException {

        Set<String> keys = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getCategoryKeys();

        // create base directory
        createBucketDirectoryIfNecessary("/", BASE_DIR);

        for (String bucketName : keys) {
            createBucketDirectoryIfNecessary("/" + BASE_DIR, bucketName);
        }
    }

    /**
     * Create a directory, rename a file away if necessary,
     *
     * @param bucketName directory to create
     */
    private void createBucketDirectoryIfNecessary(String remoteParentPath, String bucketName) throws IOException, SeafException {
        remoteParentPath = Utils.pathJoin(remoteParentPath, "/");// / -> /

        DirentWrapperModel direntWrapperModel = getDirentWrapper(repoConfig.getRepoId(), remoteParentPath);
        if (direntWrapperModel == null) {
            SLogs.d("MediaBackupScannerWorker -> createBucketDirectoryIfNecessary() -> request dirents is null");
            return;
        }


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
                renameRemoteFile(dirent.name, curPath);
            }

            break;
        }

        //if not, create bucketName dir.
        if (!found) {
            mkdirRemote(repoConfig.getRepoId(), curPath);
        }

        //
        checkAndInsert(remoteParentPath, bucketName);
    }

    private void checkAndInsert(String remoteParentPath, String bucketName) throws IOException {
        String path = Utils.pathJoin(remoteParentPath, bucketName);

        List<TransferModel> pendingList = null;
        if (!TextUtils.equals("/", remoteParentPath)) {
            pendingList = GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.getCategoryTransferList(bucketName);
        }

        if (CollectionUtils.isEmpty(pendingList)) {
            return;
        }

        DirentWrapperModel direntWrapperModel = getDirentWrapper(repoConfig.getRepoId(), path);
        if (direntWrapperModel == null) {
            SLogs.d("MediaBackupScannerWorker -> checkAndInsert() -> request dirents is null");
            return;
        }

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
                SLogs.d("skip file -> [remote exists] " + filename + ", because we have uploaded it in the past.");

                GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.remove(bucketName, transferModel);
            }
        }
    }

    private DirentWrapperModel getDirentWrapper(String repoId, String parentPath) throws IOException {
        //get parent dirent list from remote
        Call<DirentWrapperModel> direntWrapperModelCall = HttpIO.getCurrentInstance().execute(RepoService.class).getDirentsSync(repoId, parentPath);
        retrofit2.Response<DirentWrapperModel> res = direntWrapperModelCall.execute();
        if (!res.isSuccessful()) {
            SLogs.d("MediaBackupScannerWorker -> getDirentWrapper() -> request dirents failed");
            return null;
        }

        DirentWrapperModel tempWrapper = res.body();
        if (tempWrapper == null) {
            SLogs.d("MediaBackupScannerWorker -> getDirentWrapper() -> request dirents is null");
            return null;
        }

        if (!TextUtils.isEmpty(tempWrapper.error_msg)) {
            SLogs.d("MediaBackupScannerWorker -> getDirentWrapper(): " + tempWrapper.error_msg);
            return null;
        }
        return tempWrapper;
    }


    private void renameRemoteFile(String name, String fullPath) throws IOException {
        // there is already a file. move it away.
        String newFilename = getApplicationContext().getString(R.string.camera_sync_rename_file, name);

        Map<String, String> renameMap = new HashMap<>();
        renameMap.put("operation", "rename");
        renameMap.put("newname", newFilename);
        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(renameMap);

        retrofit2.Response<String> renameRes = HttpIO.getCurrentInstance()
                .execute(FileService.class)
                .renameFileCall(repoConfig.getRepoId(), fullPath, requestBodyMap)
                .execute();
        if (renameRes.isSuccessful()) {
            SLogs.d("Folder rename success：" + fullPath);
        } else {
            SLogs.d("Folder rename failed：" + fullPath);
        }
    }

}
