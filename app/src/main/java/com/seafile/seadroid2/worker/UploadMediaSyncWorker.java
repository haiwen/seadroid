package com.seafile.seadroid2.worker;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.provider.MediaStore;
import android.text.TextUtils;
import android.util.Pair;
import android.webkit.MimeTypeMap;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.google.common.base.Joiner;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.data.db.AppDatabase;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferFeature;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.data.model.repo.DirentWrapperModel;
import com.seafile.seadroid2.ui.repo.RepoService;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;
import com.seafile.seadroid2.util.FileTools;
import com.seafile.seadroid2.util.HttpUtils;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.sp.SettingsManager;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import okhttp3.RequestBody;
import retrofit2.Call;

public class UploadMediaSyncWorker extends BaseWorker {

    private final String BASE_DIR = "My Photos";
    private String targetRepoId;
    private String targetRepoName;
    private Account account;
    private Map<String, List<String>> needToUploadMap = new HashMap<>();
    private List<String> bucketIdList;

    public UploadMediaSyncWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        account = SupportAccountManager.getInstance().getCurrentAccount();
    }

    @NonNull
    @Override
    public Result doWork() {
        account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return Result.success();
        }

        SLogs.d("MediaSyncWorker start");
        try {
            loadMedia();
        } catch (SeafException | IOException e) {
            throw new RuntimeException(e);
        }

        return Result.success();
    }


    private void loadMedia() throws SeafException, IOException {
        targetRepoId = SettingsManager.getInstance().getCameraUploadRepoId();
        targetRepoName = SettingsManager.getInstance().getCameraUploadRepoName();
        bucketIdList = SettingsManager.getInstance().getCameraUploadBucketList();

        if (TextUtils.isEmpty(targetRepoId)) {
            SLogs.d("MediaSyncWorker: repoId is empty");
            return;
        }

        if (bucketIdList.isEmpty()) {
            List<GalleryBucketUtils.Bucket> allBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
            for (GalleryBucketUtils.Bucket bucket : allBuckets) {
                bucketIdList.add(bucket.id);
            }
        }

        uploadImages();

        if (SettingsManager.getInstance().isVideosUploadAllowed()) {
            uploadVideos();
        }

        if (needToUploadMap.isEmpty()) {
            SLogs.e("UploadMediaSyncWorker no need to upload");
            return;
        }

        // create directories for media buckets
        createDirectories();

        //start worker
        BackgroundJobManagerImpl.getInstance().startAlbumBackupJob();
    }

    private void uploadImages() throws IOException {
        if (isStopped())
            return;

        if (bucketIdList.isEmpty()) {
            SLogs.e("no media in local storage");
            return;
        }

        String[] selectionArgs = bucketIdList.toArray(new String[]{});
        String selection = MediaStore.Images.ImageColumns.BUCKET_ID + " IN " + varArgs(bucketIdList.size());

        Cursor cursor = getApplicationContext().getContentResolver().query(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                new String[]{
                        MediaStore.Images.Media._ID,
                        MediaStore.Images.Media.DATA,
                        MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME
                },
                selection,
                selectionArgs,
                MediaStore.Images.ImageColumns.DATE_ADDED + " DESC"
        );

        try {
            if (cursor == null) {
                return;
            }

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

    private void uploadVideos() throws IOException {
        if (isStopped())
            return;

        if (bucketIdList.isEmpty()) {
            SLogs.e("no media in local storage");
            return;
        }

        String[] selectionArgs = bucketIdList.toArray(new String[]{});
        String selection = MediaStore.Video.VideoColumns.BUCKET_ID + " IN " + varArgs(bucketIdList.size());

        Cursor cursor = getApplicationContext().getContentResolver().query(
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

        // upload them one by one
        while (!isStopped() && cursor.moveToNext()) {

            File file;

            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                if (media.equals("images")) {
                    String imageId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media._ID));
                    Uri imageUri = Uri.withAppendedPath(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, imageId);
                    if (imageUri == null) {
                        continue;
                    }

                    String p = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), imageUri, media);
                    file = new File(p);
                } else {
                    String videoId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media._ID));
                    Uri videoUri = Uri.withAppendedPath(MediaStore.Video.Media.EXTERNAL_CONTENT_URI, videoId);
                    if (videoUri == null) {
                        continue;
                    }

                    String p = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(), videoUri, media);
                    file = new File(p);
                }
            } else {
                int dataColumn = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DATA);
                if (cursor.getString(dataColumn) == null) {
                    continue;
                }
                file = new File(cursor.getString(dataColumn));
            }

            int bucketColumn = cursor.getColumnIndexOrThrow(MediaStore.Images.ImageColumns.BUCKET_DISPLAY_NAME);
            String bucketName = cursor.getString(bucketColumn);

            // local file does not exist. some inconsistency in the Media Provider? Ignore and continue
            if (!file.exists()) {
                SLogs.d("skip file -> " + file.getAbsolutePath() + ", because it doesn't exist");
                continue;
            }

            // Ignore all media by Seafile. We don't want to upload our own cached files.
            if (file.getAbsolutePath().startsWith(StorageManager.getInstance().getMediaDir().getAbsolutePath())) {
                SLogs.d("skip file -> " + file.getAbsolutePath() + ", because it's part of the Seadroid cache");
                continue;
            }

            int existsCount = AppDatabase
                    .getInstance()
                    .fileTransferDAO()
                    .checkOneByFullPath(account.getSignature(), file.getAbsolutePath(), TransferAction.UPLOAD, TransferFeature.ALBUM_BACKUP);
            if (existsCount > 0) {
                SLogs.d("skip file -> " + file.getAbsolutePath() + ", because we have uploaded it in the past.");
                continue;
            }

            SLogs.d("new file -> " + file.getAbsolutePath());

            if (!needToUploadMap.containsKey(bucketName)) {
                needToUploadMap.put(bucketName, new ArrayList<String>());
            }

            List<String> needUploadList = needToUploadMap.get(bucketName);
            needUploadList.add(file.getAbsolutePath());
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
    private void forceCreateDirectory(String parent, String dirIsBucketName, List<String> filePathList) throws IOException, SeafException {

        parent = Utils.pathJoin(parent, "/");// // -> /

        //get parent dirent list
        Call<DirentWrapperModel> direntWrapperModelCall = IO.getSingleton().execute(RepoService.class).getDirentsCall(targetRepoId, parent);
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
            mkdirRemote(targetRepoId, curPath);
        }

        if (CollectionUtils.isEmpty(filePathList)) {
            return;
        }

        //
        checkAndInsert(curPath, filePathList);
    }

    private void checkAndInsert(String parent, List<String> filePathList) throws SeafException, IOException {
        Call<DirentWrapperModel> direntWrapperModelCall = IO.getSingleton().execute(RepoService.class).getDirentsCall(targetRepoId, parent);
        retrofit2.Response<DirentWrapperModel> res = direntWrapperModelCall.execute();
        if (!res.isSuccessful()) {
            throw SeafException.networkException;
        }

        DirentWrapperModel direntWrapperModel = res.body();
        if (direntWrapperModel == null) {
            throw SeafException.networkException;
        }

        for (String absPath : filePathList) {

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

            //Check that the remote repository already has this file
            Pattern pattern = Pattern.compile(Pattern.quote(prefix) + "( \\(\\d+\\))?" + Pattern.quote(suffix));
            for (DirentModel dirent : direntWrapperModel.dirent_list) {
                if (pattern.matcher(dirent.name).matches() && dirent.size == file.length()) {
                    return;
                }
            }

            String parentPath = Utils.pathJoin(parent, "/");

            insertWorkIntoDb(file, parentPath);
        }
    }

    private void mkdirRemote(String repoId, String path) throws IOException {
        Map<String, String> requestDataMap = new HashMap<>();
        requestDataMap.put("operation", "mkdir");
        requestDataMap.put("create_parents", "true");

        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(requestDataMap);

        retrofit2.Response<String> stringResponse = IO.getSingleton()
                .execute(RepoService.class)
                .mkDirCall(repoId, path, requestBodyMap)
                .execute();

        if (stringResponse.isSuccessful()) {
            SLogs.e("创建文件夹成功：" + path);
        } else {
            SLogs.e("创建文件夹失败：" + stringResponse.errorBody().string());
        }
    }

    private void renameRemoteFile(String name, String fullPath) throws IOException {
        // there is already a file. move it away.
        String newFilename = getApplicationContext().getString(R.string.camera_sync_rename_file, name);

        Map<String, String> renameMap = new HashMap<>();
        renameMap.put("operation", "rename");
        renameMap.put("newname", newFilename);
        Map<String, RequestBody> requestBodyMap = HttpUtils.generateRequestBody(renameMap);

        retrofit2.Response<String> renameRes = IO.getSingleton()
                .execute(RepoService.class)
                .renameFileCall(targetRepoId, fullPath, requestBodyMap)
                .execute();
        if (renameRes.isSuccessful()) {
            SLogs.e("重命名文件夹成功：" + fullPath);
        } else {
            SLogs.e("重命名文件夹失败：" + fullPath);
        }
    }

    private void insertWorkIntoDb(File file, String parenPath) {
        long now = System.currentTimeMillis();

        FileTransferEntity entity = new FileTransferEntity();
        entity.full_path = file.getAbsolutePath();
        entity.target_path = parenPath;
        entity.parent_path = FileTools.getParentPath(entity.full_path);
        entity.file_name = file.getName();
        entity.file_size = file.length();
        entity.file_format = FileTools.getFileExtension(entity.full_path);
        entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.is_block = false; //album backup is not store in encrypted repo.
        entity.repo_id = targetRepoId;
        entity.repo_name = targetRepoName;
        entity.related_account = account.getSignature();
        entity.data_source = TransferFeature.ALBUM_BACKUP;
        entity.created_at = now;
        entity.modified_at = now;
        entity.action_end_at = 0;
        entity.is_update = false;
        entity.is_copy_to_local = false;
        entity.transfer_action = TransferAction.UPLOAD;
        entity.transfer_result = TransferResult.NO_RESULT;
        entity.transfer_status = TransferStatus.TRANSFER_WAITING;

        entity.uid = entity.getUID();

        //insert
        AppDatabase.getInstance().fileTransferDAO().insert(entity);
    }
}
