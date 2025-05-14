package com.seafile.seadroid2.provider;

import android.content.Context;
import android.text.TextUtils;
import android.util.Log;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.http.BaseOkHttpClient;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okio.BufferedSink;

@Todo("unstable feature, will be refactored/removed in the future")
public class OpenDocumentWriteWatcher {
    private static final long POLL_INTERVAL_MS = 200;
    private static final long MAX_WAIT_TIME_MS = 60_000;
    private static final ExecutorService executor = Executors.newCachedThreadPool();

    /**
     * Start a background thread, wait for the file to be written, and then submit the upload task to WorkManager.
     */
    public static void scheduleUploadAfterClose(Context context, Account account, String repoId, String repoName, String displayName, String targetPath, File file, String documentId) {
        executor.execute(() -> {
            try {
                Log.e("UploadWatcher", "Watching file write: " + file.getName());

                boolean completed = waitForFileStable(file);
                if (!completed) {
                    Log.e("UploadWatcher", "File write timeout, skipping upload: " + file.getName());
                    return;
                }

                Log.e("UploadWatcher", "File ready, scheduling upload: " + file.getAbsolutePath());

                uploadStreamToCloud(account, repoId, targetPath, displayName, file);
//                enqueueUploadWork(context, account, repoId, repoName, targetPath, file, documentId);

            } catch (Exception e) {
                Log.e("UploadWatcher", "Failed to watch file: " + file.getName(), e);
            }
        });
    }

    /**
     * Check whether the file is written: Check whether the file size is stable within a certain period of time.
     */
    private static boolean waitForFileStable(File file) throws InterruptedException {
        long start = System.currentTimeMillis();
        long lastSize = -1;

        while (System.currentTimeMillis() - start < MAX_WAIT_TIME_MS) {
            long currentSize = file.length();

            if (currentSize > 0 && currentSize == lastSize) {
                return true; // The size is stable and the write is considered complete
            }

            lastSize = currentSize;
            Thread.sleep(POLL_INTERVAL_MS);
        }

        return false;
    }

    private static void uploadStreamToCloud(Account account, String repoId, String targetPath, String displayName, File pfd) throws FileNotFoundException {

        String uploadUrl;
        try {
            retrofit2.Response<String> uploadUrlRes = HttpIO.getInstanceByAccount(account)
                    .execute(FileService.class)
                    .getFileUploadLink(repoId, "/")
                    .execute();

            if (!uploadUrlRes.isSuccessful()) {
                throw new FileNotFoundException("request transfer url failed");
            }

            uploadUrl = uploadUrlRes.body();
            uploadUrl = StringUtils.replace(uploadUrl, "\"", "");

            if (TextUtils.isEmpty(uploadUrl)) {
                throw new FileNotFoundException("request transfer url failed");
            }
        } catch (IOException e) {
            SLogs.e(e);
            throw new FileNotFoundException("request transfer url failed");
        }

        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        //parent_dir: / is repo root
        builder.addFormDataPart("parent_dir", "/");
        String dir = Utils.getParentPath(targetPath);
        dir = StringUtils.removeStart(dir, "/");
        builder.addFormDataPart("relative_path", dir);


        // 将 ParcelFileDescriptor 转换成 FileInputStream
        try (FileInputStream in = new FileInputStream(pfd)) {

            RequestBody body = new RequestBody() {
                @Override
                public MediaType contentType() {
                    return MediaType.parse("application/octet-stream");
                }

                @Override
                public void writeTo(BufferedSink sink) throws IOException {
                    byte[] buffer = new byte[TransferWorker.SEGMENT_SIZE];
                    int read;
                    while ((read = in.read(buffer)) != -1) {
                        // 将读取的数据实时写入 OkHttp sink（上传流）
                        sink.write(buffer, 0, read);
                    }
                }
            };
            builder.addFormDataPart("file", displayName, body);

            RequestBody requestBody = builder.build();

            Request request = new Request.Builder()
                    .url(uploadUrl)
                    .post(requestBody)
                    .build();

            BaseOkHttpClient okHttpClient = HttpIO.getInstanceByAccount(account).getOkHttpClient();
            okhttp3.Call call = okHttpClient.getOkClient().newCall(request);
            try (okhttp3.Response response = call.execute()) {
                if (response.isSuccessful()) {
                    Log.d("SeafileProvider", "Upload successful");
                } else {
                    Log.e("SeafileProvider", "Upload failed: " + response.code());
                }
            }
        } catch (IOException e) {
            Log.e("SeafileProvider", "Upload failed: " + e.getMessage());
        }
    }

    /**
     * 提交 WorkManager 上传任务
     */
    private static void enqueueUploadWork(Context context, Account account, String repoId, String repoName, String targetPath, File file, String documentId) {

        TransferModel model = new TransferModel();
        model.created_at = System.currentTimeMillis();
        model.full_path = file.getAbsolutePath();
        model.file_name = file.getName();
        model.file_size = file.length();
        model.save_to = SaveTo.DELETE;
        model.data_source = TransferDataSource.FILE_BACKUP;
        model.related_account = account.getSignature();
        model.repo_id = repoId;
        model.repo_name = repoName;
        model.target_path = targetPath;
        model.setParentPath(Utils.getParentPath(targetPath));
        model.transfer_strategy = ExistingFileStrategy.REPLACE;
        model.setId(model.genStableId());

        GlobalTransferCacheList.FILE_UPLOAD_QUEUE.put(model);

        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
    }
}
