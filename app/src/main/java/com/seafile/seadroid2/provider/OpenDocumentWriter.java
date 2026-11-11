package com.seafile.seadroid2.provider;

import android.os.CancellationSignal;
import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import retrofit2.Call;
import retrofit2.Response;

public class OpenDocumentWriter {
    private static final String TAG = "OpenDocumentWriter";

    public static void uploadFileToCloud(
            Account account, String repoId,
            String repoName, String fullPath,
            String displayName, File tempFile,
            @Nullable CancellationSignal signal) throws FileNotFoundException {

        boolean isExists = false;
        String uploadUrl;
        try {
            //check exists in remote
            Call<DirentFileModel> detailCall = HttpManager.getHttpWithAccount(account)
                    .execute(FileService.class)
                    .getFileDetailCall(repoId, fullPath);
            Response<DirentFileModel> detailRes = detailCall.execute();
            if (detailRes.isSuccessful()) {
                DirentFileModel b = detailRes.body();
                if (b != null) {
                    isExists = true;
                } else {
                    //not found in remote, can create it.
                }
            } else {
                if (detailRes.code() == HttpURLConnection.HTTP_NOT_FOUND) {
                    //not found in remote, can create it.
                } else {

                }
            }

            SLogs.d(TAG, "is exists in remote？: " + isExists);

            //get upload url
            Response<String> uploadUrlRes = HttpManager.getHttpWithAccount(account)
                    .execute(FileService.class)
                    .getFileUploadLink(repoId, "/")
                    .execute();

            if (!uploadUrlRes.isSuccessful()) {
                throw new FileNotFoundException("request transfer url failed, request is not successful");
            }

            uploadUrl = uploadUrlRes.body();
            uploadUrl = StringUtils.replace(uploadUrl, "\"", "");
            SLogs.d(TAG, "upload url: " + uploadUrl);

            if (TextUtils.isEmpty(uploadUrl)) {
                throw new FileNotFoundException("request transfer url failed, upload url is empty");
            }
        } catch (IOException e) {
            SLogs.e(e);
            FileNotFoundException exception = new FileNotFoundException("request transfer url failed, " + e.getMessage());
            exception.initCause(e);
            throw exception;
        }

        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        //parent_dir: / is repo root
        builder.addFormDataPart("parent_dir", "/");

        String dir = Utils.getParentPath(fullPath);
        dir = StringUtils.removeStart(dir, "/");
        builder.addFormDataPart("relative_path", dir);

        builder.addFormDataPart("replace", isExists ? "1" : "0");

        try {
            RequestBody fileRequestBody = RequestBody.create(MediaType.parse("application/octet-stream"), tempFile);
            builder.addFormDataPart("file", displayName, fileRequestBody);

            RequestBody buildRequestBody = builder.build();
            Request request = new Request.Builder()
                    .url(uploadUrl)
                    .post(buildRequestBody)
                    .build();

            okhttp3.Call call = HttpManager.getHttpWithAccount(account)
                    .getSafeClient()
                    .getOkClient()
                    .newCall(request);

            if (signal != null) {
                signal.setOnCancelListener(new CancellationSignal.OnCancelListener() {
                    @Override
                    public void onCancel() {
                        if (!call.isCanceled()) {
                            SLogs.d(TAG, "Upload cancelled");
                            call.cancel();
                        }
                    }
                });
            }

            try (okhttp3.Response response = call.execute()) {
                if (response.isSuccessful()) {
                    File localFile = DataManager.getLocalFileCachePath(account, repoId, repoName, fullPath);
                    Path path = Files.move(tempFile.toPath(), localFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                    boolean isSuccess = path.toFile().exists();
                    SLogs.d(TAG, "is move success? " + isSuccess);

                    try (ResponseBody body = response.body()) {
                        if (body != null) {
                            String resStr = new String(body.bytes());
                            if (TextUtils.isEmpty(resStr)) {
                                // if the returned data is abnormal due to some reason,
                                // it is set to null and uploaded when the next scan arrives
                                SLogs.d(TAG, "Upload successful", "but file id is empty", "targetPath: " + fullPath);
                            } else {
                                SLogs.d(TAG, "Upload successful", "fileId：" + resStr, "targetPath: " + fullPath);
                                String fileId = resStr.replace("\"", "");
                                saveIntoLocalDb(account, repoId, repoName, fullPath, localFile, fileId);
                            }
                        } else {
                            // if the returned data is abnormal due to some reason,
                            // it is set to null and uploaded when the next scan arrives
                            SLogs.d(TAG, "Upload successful", "but response body is empty", "targetPath: " + fullPath);
                        }
                    }
                } else {
                    String errorMessage = "";

                    ResponseBody errorBody = response.body();

                    if (errorBody != null) {
                        errorMessage = errorBody.string();
                    }

                    throw new IOException(
                            "Upload failed. HTTP code: "
                                    + response.code()
                                    + ", message: "
                                    + errorMessage
                    );
                }
            }
        } catch (IOException e) {
            SLogs.d(TAG, "Upload failed: " + e.getMessage());
            SLogs.e(e);
            FileNotFoundException exception = new FileNotFoundException("Upload failed: " + e.getMessage());
            exception.initCause(e);
            throw exception;
        }
    }


    private static void saveIntoLocalDb(Account account, String repoId, String repoName, String fullPath, File destinationFile, String fileId) {
        FileCacheStatusEntity cache = new FileCacheStatusEntity();
        cache.v = 2;//new version
        cache.repo_id = repoId;
        cache.repo_name = repoName;
        cache.related_account = account.getSignature();
        cache.file_id = fileId;
        cache.file_name = destinationFile.getName();
        cache.created_at = System.currentTimeMillis();
        cache.modified_at = cache.created_at;
        cache.target_path = destinationFile.getAbsolutePath();
        cache.full_path = fullPath;
        cache.setParent_path(Utils.getParentPath(fullPath));
        cache.file_size = destinationFile.length();
        cache.file_format = FileUtils.getFileExtension(fullPath);
        cache.file_md5 = FileUtils.getFileMD5ToString(destinationFile).toLowerCase();
        cache.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(cache.file_format);
        cache.uid = cache.genUID();

        AppDatabase.getInstance().fileCacheStatusDAO().insert(cache);
        SLogs.d(TAG, "save into local db success", "fileId: " + fileId, "md5: " + cache.file_md5, "fullPath: " + fullPath);
    }
}
