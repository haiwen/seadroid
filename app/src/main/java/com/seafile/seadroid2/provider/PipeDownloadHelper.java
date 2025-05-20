package com.seafile.seadroid2.provider;

import android.os.CancellationSignal;
import android.os.ParcelFileDescriptor;
import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.io.output.TeeOutputStream;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import okhttp3.Call;
import okhttp3.Request;
import okhttp3.Response;

/**
 * Load a file from the Seafile server.
 * <p>
 * We implement the data channel via "createPipe()" to make it an asynchronous transfer mode,
 * where data is written to the pipeline in real time when data is downloaded via OkHttp
 */
public class PipeDownloadHelper {

    public interface ProgressListener {
        void onComplete();

        void onError(Exception e);
    }

    public static ParcelFileDescriptor streamDownloadToPipe(
            Account account,
            String repoId,
            String repoName,
            String remoteFullPath,
            String fileId,
            @Nullable CancellationSignal signal,
            @Nullable ProgressListener listener
    ) throws IOException {

        ParcelFileDescriptor[] pipe = ParcelFileDescriptor.createPipe();
        final ParcelFileDescriptor readFd = pipe[0];  // 返回给系统，交给第三方 App，用于"读取数据"
        final ParcelFileDescriptor writeFd = pipe[1]; // 你来写入下载内容，自己使用，用于"写入数据"

        CompletableFuture.runAsync(() -> {

            File destinationFile = DataManager.getLocalFileCachePath(account, repoId, repoName, remoteFullPath);
            TeeOutputStream teeOut = null;

            try {
                OutputStream pipeOut = new FileOutputStream(writeFd.getFileDescriptor());
                OutputStream fileOut = new FileOutputStream(destinationFile);
                teeOut = new TeeOutputStream(pipeOut, fileOut);

                downloadFile(account, repoId, repoName, remoteFullPath, teeOut, signal);

                saveIntoLocalDb(repoId, repoName, account.getSignature(), remoteFullPath, destinationFile, fileId);

                listenerIfNotNull(listener, ProgressListener::onComplete);

            } catch (Exception e) {
                listenerIfNotNull(listener, l -> l.onError(e));
            } finally {
                try {
                    if (teeOut != null) {
                        teeOut.close();
                    }
                    writeFd.close();
                } catch (IOException ignored) {
                }
            }
        });

        return readFd;
    }

    private static void downloadFile(
            Account account,
            String repoId,
            String repoName,
            String path,
            OutputStream out,
            @Nullable CancellationSignal signal
    ) throws IOException {

        String url = buildDownloadUrl(account, repoId, path);

        HttpIO httpIo = HttpIO.getInstanceByAccount(account);
        if (httpIo == null) {
            throw new FileNotFoundException();
        }

        Request request = new Request.Builder().url(url).get().build();
        Call call = httpIo.getOkHttpClient().getOkClient().newCall(request);

        if (signal != null) {
            signal.setOnCancelListener(call::cancel);
        }

        Response response = call.execute();

        try (response; InputStream in = response.body().byteStream()) {
            if (!response.isSuccessful()) {
                throw new IOException("Download failed: " + response.code());
            }
            byte[] buffer = new byte[8192];
            int read;
            long total = 0;

            while ((read = in.read(buffer)) != -1) {
                out.write(buffer, 0, read);
                total += read;
            }

            out.flush();
        }
    }

    private static void listenerIfNotNull(ProgressListener listener, Consumer<ProgressListener> call) {
        if (listener != null) {
            call.accept(listener);
        }
    }

    private static String buildDownloadUrl(Account account, String repoId, String path) throws FileNotFoundException {

        try {
            HttpIO httpIo = HttpIO.getInstanceByAccount(account);
            if (httpIo == null) {
                throw new FileNotFoundException();
            }

            retrofit2.Call<String> urlCall = httpIo.execute(FileService.class)
                    .getFileDownloadLinkSync(repoId, path, 1);

            retrofit2.Response<String> downloadUrlResp = urlCall.execute();
            if (!downloadUrlResp.isSuccessful()) {
                throw new FileNotFoundException();
            }

            String downloadUrl = downloadUrlResp.body();
            if (TextUtils.isEmpty(downloadUrl)) {
                throw new FileNotFoundException();
            }

            return downloadUrl;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static void saveIntoLocalDb(String repoId, String repoName, String relatedAccount, String remoteFullPath, File destinationFile, String fileId) {

        FileCacheStatusEntity entity = new FileCacheStatusEntity();
        entity.v = 2;//new version
        entity.repo_id = repoId;
        entity.repo_name = repoName;
        entity.related_account = relatedAccount;
        entity.file_name = destinationFile.getName();
        entity.file_id = fileId;

        entity.target_path = destinationFile.getAbsolutePath();
        entity.full_path = remoteFullPath;
        entity.setParent_path(Utils.getParentPath(remoteFullPath));

        entity.file_size = destinationFile.length();
        entity.file_format = FileUtils.getFileExtension(entity.full_path);
        entity.file_md5 = FileUtils.getFileMD5ToString(entity.target_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.created_at = System.currentTimeMillis();
        entity.modified_at = entity.created_at;

        entity.uid = entity.getUID();

        AppDatabase.getInstance().fileCacheStatusDAO().insert(entity);
    }

    // 用于同时输出到 pipe + 本地文件


}
