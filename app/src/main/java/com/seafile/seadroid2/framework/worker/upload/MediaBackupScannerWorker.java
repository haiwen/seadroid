package com.seafile.seadroid2.framework.worker.upload;

import static com.seafile.seadroid2.config.Constants.PERIODIC_SCAN_INTERVALS;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.provider.MediaStore;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.work.Data;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.common.base.Joiner;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.notification.AlbumBackupScanNotificationHelper;
import com.seafile.seadroid2.framework.util.HttpUtils;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;
import com.seafile.seadroid2.ui.file.FileService;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.repo.RepoService;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;

import kotlin.Triple;
import okhttp3.RequestBody;
import retrofit2.Call;

/**
 * Worker Tag:
 *
 * @see BackgroundJobManagerImpl#TAG_ALL
 * @see BackgroundJobManagerImpl#TAG_TRANSFER
 */
public class MediaBackupScannerWorker extends TransferWorker {
    public static final UUID UID = UUID.nameUUIDFromBytes(MediaBackupScannerWorker.class.getSimpleName().getBytes());

    private final AlbumBackupScanNotificationHelper albumNotificationHelper;

    private final String BASE_DIR = "My Photos";

    private RepoConfig repoConfig;
    private Account account;
    private final Map<String, List<Triple<String, String, Long>>> needToUploadMap = new HashMap<>();
    private List<String> bucketIdList;

    public MediaBackupScannerWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        albumNotificationHelper = new AlbumBackupScanNotificationHelper(context);

        account = SupportAccountManager.getInstance().getCurrentAccount();
    }

    @NonNull
    @Override
    public Result doWork() {
        account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        boolean canScan = checkCanScan();
        if (!canScan) {
            SLogs.d("UploadMediaScanWorker: do not start the media scan task this time");
            return Result.success(getOutData());
        }

        //todo
        String title = getApplicationContext().getString(R.string.settings_camera_upload_info_title);
        String subTitle = getApplicationContext().getString(R.string.is_scanning);

        ForegroundInfo foregroundInfo = albumNotificationHelper.getForegroundNotification(title, subTitle);
        showForegroundAsync(foregroundInfo);

        SLogs.d("MediaSyncWorker start");
        try {

            sendEvent(TransferEvent.EVENT_SCANNING, TransferDataSource.ALBUM_BACKUP);

            loadMedia();

        } catch (SeafException | IOException e) {
            SLogs.e("MediaBackupScannerWorker has occurred error", e);
        } finally {
            AlbumBackupSharePreferenceHelper.writeLastScanTime(System.currentTimeMillis());
        }

        return Result.success(getOutData());
    }

    private boolean checkCanScan() {
        boolean isEnable = AlbumBackupSharePreferenceHelper.readBackupSwitch();
        if (!isEnable) {
            return false;
        }

        boolean isForce = getInputData().getBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, false);
        if (isForce) {
            return true;
        }

        Long lastScanTime = AlbumBackupSharePreferenceHelper.readLastScanTime();
        if (lastScanTime != null && lastScanTime != 0) {
            long now = System.currentTimeMillis();
            if (now - lastScanTime < PERIODIC_SCAN_INTERVALS) {
                return false;
            }
        }

        return true;
    }

    private Data getOutData() {
        return new Data.Builder()
                .putString(TransferWorker.KEY_DATA_EVENT, TransferEvent.EVENT_SCAN_END)
                .putString(TransferWorker.KEY_DATA_TYPE, String.valueOf(TransferDataSource.ALBUM_BACKUP))
                .build();
    }

    private void loadMedia() throws SeafException, IOException {

        repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        bucketIdList = AlbumBackupSharePreferenceHelper.readBucketIds();

        if (repoConfig == null) {
            SLogs.d("MediaSyncWorker: repoConfig is null");

            sendEvent(TransferEvent.EVENT_FINISH, TransferDataSource.ALBUM_BACKUP);
            return;
        }


        if (CollectionUtils.isEmpty(bucketIdList)) {
            List<GalleryBucketUtils.Bucket> allBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
            for (GalleryBucketUtils.Bucket bucket : allBuckets) {
                bucketIdList.add(bucket.id);
            }
        }

        uploadImages();

        if (AlbumBackupSharePreferenceHelper.readAllowVideoSwitch()) {
            uploadVideos();
        }

        if (needToUploadMap.isEmpty()) {
            SLogs.d("UploadMediaSyncWorker no need to upload");
            return;
        }

        // create directories for media buckets
        createDirectories();
    }

    private void uploadImages() throws IOException {
        if (isStopped())
            return;

        if (bucketIdList.isEmpty()) {
            SLogs.d("no media in local storage");
            return;
        }

        String[] selectionArgs = bucketIdList.toArray(new String[]{});
        String selection = MediaStore.Images.ImageColumns.BUCKET_ID + " IN " + varArgs(bucketIdList.size());

//        FileTransferEntity transferEntity = AppDatabase
//                .getInstance()
//                .fileTransferDAO()
//                .getLastOneForMedia(account.getSignature(), "image/%"); //mime_type start with image/
//
//        if (transferEntity != null && transferEntity.file_original_modified_at != 0) {
//            selection += " and " + MediaStore.Images.Media.DATE_ADDED + " >= ? "; // If it's >=, might get some duplicate data
//            selectionArgs = Arrays.copyOf(selectionArgs, selectionArgs.length + 1);
//            selectionArgs[selectionArgs.length - 1] = String.valueOf(transferEntity.file_original_modified_at / 1000);
//        }

        Cursor cursor = getApplicationContext().getContentResolver().query(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                new String[]{
                        MediaStore.Images.Media._ID,
                        MediaStore.Images.Media.DATA,
                        MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME,
                        MediaStore.Images.ImageColumns.DATE_ADDED
                },
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
            iterateCursor(cursor, "images");
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
    }

    private void uploadVideos() {
        if (isStopped())
            return;

        if (bucketIdList.isEmpty()) {
            SLogs.d("no media in local storage");
            return;
        }

        String[] selectionArgs = bucketIdList.toArray(new String[]{});
        String selection = MediaStore.Video.VideoColumns.BUCKET_ID + " IN " + varArgs(bucketIdList.size());

//        FileTransferEntity transferEntity = AppDatabase
//                .getInstance()
//                .fileTransferDAO()
//                .getLastOneForMedia(account.getSignature(), "video/%"); //mime_type start with image/
//
//        if (transferEntity != null && transferEntity.file_original_modified_at != 0) {
//            selection += " and " + MediaStore.Images.Media.DATE_ADDED + " >= ? "; // If it's >=, might get some duplicate data
//            selectionArgs = Arrays.copyOf(selectionArgs, selectionArgs.length + 1);
//            selectionArgs[selectionArgs.length - 1] = String.valueOf(transferEntity.file_original_modified_at / 1000);
//        }

        Cursor cursor = getApplicationContext().getContentResolver().query(
                MediaStore.Video.Media.EXTERNAL_CONTENT_URI,
                new String[]{
                        MediaStore.Video.Media._ID,
                        MediaStore.Video.Media.DATA,
                        MediaStore.Video.VideoColumns.BUCKET_DISPLAY_NAME,
                        MediaStore.Images.ImageColumns.DATE_ADDED
                },
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
            iterateCursor(cursor, "video");
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
    private void iterateCursor(Cursor cursor, String media) {

//        String repoId = repoConfig.getRepoID();
//        String repoName = repoConfig.getRepoName();
//        File localRepoFilePath = DataManager.getLocalRepoPath(account, repoId, repoName);
//        String localRepoPhotoPath = Utils.pathJoin(localRepoFilePath.getAbsolutePath(), BASE_DIR);

        String localCacheAbsPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();

        // upload them one by one
        while (!isStopped() && cursor.moveToNext()) {

            File file;
            long dateAdded = 0;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                if (media.equals("images")) {
                    String imageId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media._ID));
                    Uri imageUri = Uri.withAppendedPath(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, imageId);
                    if (imageUri == null) {
                        continue;
                    }

                    int dateAddIndex = cursor.getColumnIndex(MediaStore.Images.Media.DATE_ADDED);
                    dateAdded = cursor.getLong(dateAddIndex);

                    String p = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), imageUri, media);
                    file = new File(p);

                } else {
                    String videoId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media._ID));
                    Uri videoUri = Uri.withAppendedPath(MediaStore.Video.Media.EXTERNAL_CONTENT_URI, videoId);
                    if (videoUri == null) {
                        continue;
                    }

                    int dateAddIndex = cursor.getColumnIndex(MediaStore.Images.Media.DATE_ADDED);
                    dateAdded = cursor.getLong(dateAddIndex);

                    String p = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), videoUri, media);
                    file = new File(p);
                }
            } else {
                int dataColumn = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATA);
                if (cursor.getString(dataColumn) == null) {
                    continue;
                }

                int dateAddIndex = cursor.getColumnIndex(MediaStore.Images.Media.DATE_ADDED);
                dateAdded = cursor.getLong(dateAddIndex);

                file = new File(cursor.getString(dataColumn));
            }

            int bucketColumn = cursor.getColumnIndexOrThrow(MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME);
            String bucketName = cursor.getString(bucketColumn);

            // local file does not exist. some inconsistency in the Media Provider? Ignore and continue
            if (!file.exists()) {
                SLogs.d("skip file -> " + file.getAbsolutePath() + ", because it doesn't exist");
                continue;
            }


//            boolean isCached = FileUtils.isFileExists(Utils.pathJoin(localRepoPhotoPath,file.getName()));
            // Ignore all media by Seafile. We don't want to upload our own cached files.
            if (file.getAbsolutePath().startsWith(localCacheAbsPath)) {
                SLogs.d("skip file -> " + file.getAbsolutePath() + ", because it's part of the Seadroid cache");
                continue;
            }

            int existsCount = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .checkOneByFullPath(repoConfig.getRepoID(), file.getAbsolutePath(), TransferAction.UPLOAD, TransferDataSource.ALBUM_BACKUP);

            if (existsCount > 0) {
                SLogs.d("skip file -> " + file.getAbsolutePath() + ", because we have uploaded it in the past.");
                continue;
            }

            SLogs.d("new file -> " + file.getAbsolutePath());

            if (!needToUploadMap.containsKey(bucketName)) {
                needToUploadMap.put(bucketName, new ArrayList<>());
            }

            //seconds -> mills
            dateAdded *= 1000;

            List<Triple<String, String, Long>> needUploadList = needToUploadMap.get(bucketName);
            needUploadList.add(new Triple<>(file.getAbsolutePath(), media, dateAdded));
            needToUploadMap.put(bucketName, needUploadList);
        }
    }

    /**
     * Create all the subdirectories on the server for the buckets that are about to be uploaded.
     *
     * @throws SeafException
     */
    private void createDirectories() throws SeafException, IOException {
        Set<String> keys = needToUploadMap.keySet();

        // create base directory
        forceCreateDirectory("/", BASE_DIR, null);

        for (String bucketName : keys) {
            forceCreateDirectory("/" + BASE_DIR, bucketName, needToUploadMap.get(bucketName));
        }
    }


    /**
     * Create a directory, rename a file away if necessary,
     *
     * @param parent          parent dir
     * @param dirIsBucketName directory to create
     */
    private void forceCreateDirectory(String parent, String dirIsBucketName, List<Triple<String, String, Long>> filePathList) throws IOException, SeafException {

        parent = Utils.pathJoin(parent, "/");// // -> /

        //todo 使用获取 path 详情接口
        //get parent dirent list
        Call<DirentWrapperModel> direntWrapperModelCall = HttpIO.getCurrentInstance().execute(RepoService.class).getDirentsSync(repoConfig.getRepoID(), parent);
        retrofit2.Response<DirentWrapperModel> res = direntWrapperModelCall.execute();
        if (!res.isSuccessful()) {
            throw SeafException.networkException;
        }

        DirentWrapperModel direntWrapperModel = res.body();
        if (direntWrapperModel == null) {
            throw SeafException.networkException;
        }

        List<DirentModel> parentDirentList = direntWrapperModel.dirent_list;

        String curPath = Utils.pathJoin(parent, dirIsBucketName);

        boolean found = false;

        //check whether the file in the parent directory contains the bucket name
        for (DirentModel dirent : parentDirentList) {
            if (!TextUtils.equals(dirent.name, dirIsBucketName)) {
                continue;
            }

            //same folder name
            if (dirent.isDir()) {
                found = true;
                continue;
            }

            //if a file same with the bucketName, rename it
            renameRemoteFile(dirent.name, curPath);
        }

        //if not, create bucketName dir.
        if (!found) {
            mkdirRemote(repoConfig.getRepoID(), curPath);
        }

        if (CollectionUtils.isEmpty(filePathList)) {
            return;
        }

        //
        checkAndInsert(curPath, filePathList);
    }

    /**
     * @param filePathList (file.getAbsolutePath(), media(image or video), dateAdded(long))
     */
    private void checkAndInsert(String parent, List<Triple<String, String, Long>> filePathList) throws SeafException, IOException {
        Call<DirentWrapperModel> direntWrapperModelCall = HttpIO.getCurrentInstance().execute(RepoService.class).getDirentsSync(repoConfig.getRepoID(), parent);
        retrofit2.Response<DirentWrapperModel> res = direntWrapperModelCall.execute();
        if (!res.isSuccessful()) {
            throw SeafException.networkException;
        }

        DirentWrapperModel direntWrapperModel = res.body();
        if (direntWrapperModel == null) {
            throw SeafException.networkException;
        }

        List<FileTransferEntity> transferList = CollectionUtils.newArrayList();
        for (Triple<String, String, Long> absPathPair : filePathList) {
            String absPath = absPathPair.getFirst();
            File file = new File(absPath);

            /*
             * We don't want to upload a file twice unless the local and remote files differ.
             *
             * It would be cool if the API2 offered a way to query the hash of a remote file.
             * Currently, comparing the file size is the best we can do.
             */
            String filename = file.getName();
            String prefix = filename.substring(0, filename.lastIndexOf("."));
            String suffix = filename.substring(filename.lastIndexOf("."));

            boolean isRemoteExists = false;
            //Check that the remote repository already has this file
            Pattern pattern = Pattern.compile(Pattern.quote(prefix) + "( \\(\\d+\\))?" + Pattern.quote(suffix));
            for (DirentModel dirent : direntWrapperModel.dirent_list) {
                if (pattern.matcher(dirent.name).matches() && dirent.size == file.length()) {
                    isRemoteExists = true;
                    break;
                }
            }

            String parentPath = Utils.pathJoin(parent, "/");

            FileTransferEntity fileTransferEntity = FileTransferEntity.convert2ThisForUploadMediaSyncWorker(account, repoConfig.getRepoID(), repoConfig.getRepoName(), file, parentPath, absPathPair.getThird(), isRemoteExists);
            transferList.add(fileTransferEntity);
        }

        if (!CollectionUtils.isEmpty(transferList)) {
            AppDatabase.getInstance().fileTransferDAO().insertAll(transferList);
        }
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
                .renameFileCall(repoConfig.getRepoID(), fullPath, requestBodyMap)
                .execute();
        if (renameRes.isSuccessful()) {
            SLogs.d("Folder rename success：" + fullPath);
        } else {
            SLogs.d("Folder rename failed：" + fullPath);
        }
    }

}
