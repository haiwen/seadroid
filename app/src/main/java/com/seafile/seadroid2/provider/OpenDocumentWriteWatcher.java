package com.seafile.seadroid2.provider;

import android.text.TextUtils;
import android.util.Log;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;

import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.Request;
import okhttp3.RequestBody;
import okio.BufferedSink;
import retrofit2.Call;
import retrofit2.Response;

public class OpenDocumentWriteWatcher {
    public static void uploadStreamToCloud(Account account, String repoId, String targetPath, String displayName, InputStream in) throws FileNotFoundException {

        boolean isExists = false;
        String uploadUrl;
        try {
            //check exists in remote
            Call<DirentFileModel> detailCall = HttpIO.getInstanceByAccount(account)
                    .execute(FileService.class)
                    .getFileDetailCall(repoId, targetPath);
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

            SLogs.e("存在？: " + isExists);

            //get upload url
            retrofit2.Response<String> uploadUrlRes = HttpIO.getInstanceByAccount(account)
                    .execute(FileService.class)
                    .getFileUploadLink(repoId, "/")
                    .execute();

            if (!uploadUrlRes.isSuccessful()) {
                throw new FileNotFoundException("request transfer url failed");
            }

            uploadUrl = uploadUrlRes.body();
            uploadUrl = StringUtils.replace(uploadUrl, "\"", "");
            SLogs.e("上传链接？: " + uploadUrl);

            if (TextUtils.isEmpty(uploadUrl)) {
                throw new FileNotFoundException("request transfer url failed");
            }
        } catch (IOException e) {
            SLogs.e(e);
            throw new FileNotFoundException("request transfer url failed");
        }

        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

//        if (isExists) {
//            builder.addFormDataPart("target_file", targetPath);
//        } else {
//
//        }

        //parent_dir: / is repo root
        builder.addFormDataPart("parent_dir", "/");

        String dir = Utils.getParentPath(targetPath);
        dir = StringUtils.removeStart(dir, "/");
        builder.addFormDataPart("relative_path", dir);

        builder.addFormDataPart("replace", isExists ? "1" : "0");

        try {
            RequestBody body = new RequestBody() {
                @Override
                public MediaType contentType() {
                    return MediaType.parse("application/octet-stream");
                }

                @Override
                public long contentLength() {
                    return -1;
                }

                @Override
                public void writeTo(@NonNull BufferedSink sink) throws IOException {
                    byte[] buffer = new byte[TransferWorker.SEGMENT_SIZE];
                    int readCount;
                    long total = 0;
                    while ((readCount = in.read(buffer)) != -1) {
                        sink.write(buffer, 0, readCount);
                        total += readCount;
                    }
                    SLogs.e("Total bytes uploaded: " + total);

                    sink.flush();
                }
            };
            builder.addFormDataPart("file", displayName, body);

            RequestBody requestBody = builder.build();
            Request request = new Request.Builder()
                    .url(uploadUrl)
                    .post(requestBody)
                    .build();

            okhttp3.Call call = HttpIO.getInstanceByAccount(account)
                    .getOkHttpClient()
                    .getOkClient()
                    .newCall(request);

            try (okhttp3.Response response = call.execute()) {
                if (response.isSuccessful()) {
                    SLogs.e("Upload successful");
                } else {
                    SLogs.e("Upload failed: " + response.body().string());
                }
            }
        } catch (IOException e) {
            Log.e("SeafileProvider", "Upload failed: " + e.getMessage());
        }
    }
}
