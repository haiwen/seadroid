package com.seafile.seadroid2.framework.service;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CloneUtils;
import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileBackupStatusEntity;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.http.HttpManager;
import com.seafile.seadroid2.framework.motionphoto.MotionPhotoDescriptor;
import com.seafile.seadroid2.framework.motionphoto.MotionPhotoDetector;
import com.seafile.seadroid2.framework.notification.GeneralNotificationHelper;
import com.seafile.seadroid2.framework.util.ExceptionUtils;
import com.seafile.seadroid2.framework.util.FileUtils;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.body.FileChunkRequestBody;
import com.seafile.seadroid2.framework.worker.body.FileStreamRequestBody;
import com.seafile.seadroid2.framework.worker.body.UriStreamRequestBody;
import com.seafile.seadroid2.framework.worker.body.UriChunkRequestBody;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.jni.HeicNative;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.ui.file.FileService;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;

import okhttp3.Call;
import okhttp3.Headers;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;

public abstract class ParentEventUploader extends ParentEventTransfer {
    public static final String TAG = "Foreground-Thread-Uploader";//ParentEventUploader
    private static final long CHUNK_SIZE_BYTES = 8 * 1024 * 1024L; // 8MB
    private static final long CHUNK_TRIGGER_SIZE_BYTES = 1024 * 1024 * 1024L; // 1GB
    private static final long CHUNK_UPLOAD_READ_TIMEOUT_SECONDS = 600L;

    public ParentEventUploader(Context context, ITransferNotification n) {
        super(context, n);
        _fileTransferProgressListener.setProgressListener(progressListener);
    }

    public abstract FeatureDataSource getFeatureDataSource();

    /**
     * listener
     */
    private final FileTransferProgressListener _fileTransferProgressListener = new FileTransferProgressListener();

    /**
     * progress listener
     */
    private final FileTransferProgressListener.TransferProgressListener progressListener = new FileTransferProgressListener.TransferProgressListener() {
        @Override
        public void onProgressNotify(TransferModel transferModel, int percent, long transferredSize, long totalSize) {
            SafeLogs.d(TAG, "onProgressNotify()", "UPLOAD: " + transferModel.file_name + " -> progress：" + percent);

            transferModel.transferred_size = transferredSize;
            GlobalTransferCacheList.updateTransferModel(transferModel);

            notifyProgress(transferModel.file_name, percent);

            sendProgressEvent(getFeatureDataSource(), transferModel);
        }
    };

    private void notifyProgress(String fileName, int percent) {
        if (getTransferNotificationDispatcher() != null) {
            getTransferNotificationDispatcher().showProgress(getFeatureDataSource(), fileName, percent);
        }
    }

    public void notifyError(SeafException seafException) {
        if (seafException == SeafException.OUT_OF_QUOTA) {
            getGeneralNotificationHelper().showErrorNotification(R.string.above_quota);
        } else if (seafException == SeafException.NETWORK_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.network_error);
        } else if (seafException == SeafException.NOT_FOUND_USER_EXCEPTION) {
            getGeneralNotificationHelper().showErrorNotification(R.string.saf_account_not_found_exception);
        } else if (seafException == SeafException.USER_CANCELLED_EXCEPTION) {
            //do nothing
        } else {
            getGeneralNotificationHelper().showErrorNotification(seafException.getMessage());
        }
    }

    private GeneralNotificationHelper generalNotificationHelper;

    public GeneralNotificationHelper getGeneralNotificationHelper() {
        if (generalNotificationHelper == null) {
            this.generalNotificationHelper = new GeneralNotificationHelper(getContext());
        }
        return generalNotificationHelper;
    }

    private TransferModel currentTransferModel;
    private Call newCall;

    private UriStreamRequestBody uriRequestBody;
    private UriChunkRequestBody uriChunkRequestBody;
    private FileChunkRequestBody fileChunkRequestBody;
    private FileStreamRequestBody fileRequestBody;

    private boolean isStop = false;
    private OkHttpClient primaryHttpClient;
    private OkHttpClient chunkUploadHttpClient;

    private static class UploadSource {
        @Nullable
        final File uploadFile;
        @Nullable
        final Uri uploadUri;
        final boolean uploadFromUri;
        final long createdTime;
        final long fileSize;

        UploadSource(@Nullable File uploadFile, @Nullable Uri uploadUri, boolean uploadFromUri, long createdTime, long fileSize) {
            this.uploadFile = uploadFile;
            this.uploadUri = uploadUri;
            this.uploadFromUri = uploadFromUri;
            this.createdTime = createdTime;
            this.fileSize = fileSize;
        }
    }

    public OkHttpClient getPrimaryHttpClient(Account account) {
        if (primaryHttpClient == null) {
            primaryHttpClient = HttpManager.getHttpWithAccount(account).getSafeClient().getOkClient();
        }
        return primaryHttpClient;
    }

    public TransferModel getCurrentTransferringModel() {
        return currentTransferModel;
    }

    private OkHttpClient getChunkUploadHttpClient(Account account) {
        if (chunkUploadHttpClient == null) {
            chunkUploadHttpClient = getPrimaryHttpClient(account)
                    .newBuilder()
                    .readTimeout(CHUNK_UPLOAD_READ_TIMEOUT_SECONDS, TimeUnit.SECONDS)
                    .build();
        }
        return chunkUploadHttpClient;
    }

    /**
     * Stop downloading the model
     * <p>
     * the model is in the downloading, it will be stopped.
     */
    public void stopThis() {
        SafeLogs.d(TAG, "stopThis()", getFeatureDataSource().name());
        isStop = true;

        if (uriRequestBody != null) {
            uriRequestBody.setStop(true);
        }

        if (uriChunkRequestBody != null) {
            uriChunkRequestBody.setStop(true);
        }

        if (fileRequestBody != null) {
            fileRequestBody.setStop(true);
        }

        if (fileChunkRequestBody != null) {
            fileChunkRequestBody.setStop(true);
        }

        if (primaryHttpClient != null) {
            primaryHttpClient.dispatcher().cancelAll();
        }

        if (chunkUploadHttpClient != null) {
            chunkUploadHttpClient.dispatcher().cancelAll();
        }

        if (newCall != null) {
            newCall.cancel();
        }

        if (getTransferNotificationDispatcher() != null) {
            getTransferNotificationDispatcher().clearDelay();
        }
    }

    public void transfer(Account account, TransferModel transferModel) throws SeafException {
        try {
            if (isStop) {
                isStop = false;
            }

            currentTransferModel = CloneUtils.deepClone(transferModel, TransferModel.class);
            SafeLogs.d(TAG, "transfer start, model:");
            SafeLogs.d(TAG, currentTransferModel.toString());

            transferFile(account);

            sendProgressCompleteEvent(getFeatureDataSource(), currentTransferModel);

        } catch (SeafException seafException) {
            // update db
            updateToFailed(seafException.getMessage());

            //send an event, update transfer entity first.
            sendProgressCompleteEvent(getFeatureDataSource(), currentTransferModel);
            throw seafException;
        }
    }


    private void transferFile(Account account) throws SeafException {
        if (account == null) {
            SafeLogs.d(TAG, "transferFile()", "account is null, can not upload file");
            throw SeafException.NOT_FOUND_USER_EXCEPTION;
        }

        if (TextUtils.isEmpty(account.token)) {
            SafeLogs.d(TAG, "transferFile()", "account is not logged in : " + account);
            throw SeafException.UNAUTHORIZED_EXCEPTION;
        }

        SafeLogs.d(TAG, "transferFile()", "start transfer, local file path: " + currentTransferModel.full_path);

        UploadSource source = resolveUploadSource();
        notifyTransferStarted();

        String uploadUrl = getFileUploadUrl(account,
                currentTransferModel.repo_id,
                currentTransferModel.getParentPath(),
                currentTransferModel.transfer_strategy == ExistingFileStrategy.REPLACE);

        boolean useChunkedUpload = shouldUseChunkedUpload(source);
        if (useChunkedUpload) {
            String chunkUploadUrl = appendRetJsonParam(uploadUrl);
            int totalChunks = (int) Math.ceil((double) source.fileSize / CHUNK_SIZE_BYTES);
            SafeLogs.d(TAG, "transferFile()", "using chunked upload, total chunks: " + totalChunks);
            for (int i = 0; i < totalChunks; i++) {
                if (isStop) {
                    throw SeafException.USER_CANCELLED_EXCEPTION;
                }
                long offset = i * CHUNK_SIZE_BYTES;
                long currentChunkSize = Math.min(CHUNK_SIZE_BYTES, source.fileSize - offset);
                boolean isLastChunk = i == totalChunks - 1;
                int chunkNumber = i + 1;
                uploadSingleChunk(account, source.createdTime, offset, currentChunkSize, source.fileSize, CHUNK_SIZE_BYTES, true, isLastChunk,
                        source.uploadFile, source.uploadUri, source.uploadFromUri, chunkNumber, totalChunks, chunkUploadUrl);
            }
        } else {
            uploadSingleChunk(account, source.createdTime, 0, source.fileSize, source.fileSize, source.fileSize, false, true,
                    source.uploadFile, source.uploadUri, source.uploadFromUri, 1, 1, uploadUrl);
        }
    }

    private UploadSource resolveUploadSource() throws SeafException {
        long createdTime = -1;
        File uploadFile = null;
        Uri uploadUri = null;
        boolean uploadFromUri = false;

        currentTransferModel.motion_photo_path = convertJpegToHeicIfMotionPhoto();
        if (currentTransferModel.hasExtraMotionPhoto()) {
            File heicFile = new File(currentTransferModel.motion_photo_path);
            String fileName = FileUtils.getBaseName(currentTransferModel.file_name) + ".heic";
            currentTransferModel.original_name = currentTransferModel.file_name;
            currentTransferModel.file_name = fileName;
            currentTransferModel.target_path = Utils.getFullPath(currentTransferModel.target_path) + currentTransferModel.file_name;
            uploadFile = heicFile;
        } else if (currentTransferModel.full_path.startsWith("content://")) {
            uploadUri = Uri.parse(currentTransferModel.full_path);
            uploadFromUri = true;
            boolean isHasPermission = FileUtils.isUriHasPermission(getContext(), uploadUri);
            if (!isHasPermission) {
                throw SeafException.PERMISSION_EXCEPTION;
            }
        } else {
            uploadFile = new File(currentTransferModel.full_path);
            if (!uploadFile.exists()) {
                throw SeafException.NOT_FOUND_EXCEPTION;
            }
        }

        if (currentTransferModel.full_path.startsWith("content://")) {
            Uri fileUri = Uri.parse(currentTransferModel.full_path);
            currentTransferModel.file_size = FileUploadUtils.resolveSize(getContext(), fileUri);
            createdTime = FileUtils.getCreatedTimeFromUri(getContext(), fileUri);
        } else {
            File originalFile = new File(currentTransferModel.full_path);
            createdTime = FileUtils.getCreatedTimeFromPath(getContext(), originalFile);
        }

        return new UploadSource(uploadFile, uploadUri, uploadFromUri, createdTime, currentTransferModel.file_size);
    }

    private void notifyTransferStarted() {
        currentTransferModel.transferred_size = 0;
        currentTransferModel.transfer_status = TransferStatus.IN_PROGRESS;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        _fileTransferProgressListener.setCurrentTransferModel(currentTransferModel);
        sendProgressEvent(getFeatureDataSource(), currentTransferModel);
        notifyProgress(currentTransferModel.file_name, 0);
    }

    /**
     * Should use chunked upload?
     * <p>
     * - file size must be known<br>
     * - file size must be larger than CHUNK_TRIGGER_SIZE_BYTES<br>
     * - uri must be able to seek<br>
     * */
    private boolean shouldUseChunkedUpload(@NonNull UploadSource source) {
        boolean hasKnownLength = source.fileSize > 0;
        boolean useChunkedUpload = hasKnownLength && source.fileSize > CHUNK_TRIGGER_SIZE_BYTES;
        if (useChunkedUpload && source.uploadFromUri && source.uploadUri != null && !canSeekUri(source.uploadUri)) {
            SafeLogs.d(TAG, "transferFile()", "uri does not support seek, fallback to stream upload: " + source.uploadUri);
            useChunkedUpload = false;
        }
        return useChunkedUpload;
    }

    private boolean canSeekUri(@NonNull Uri uri) {
        try (android.os.ParcelFileDescriptor pfd = getContext().getContentResolver().openFileDescriptor(uri, "r")) {
            if (pfd == null) {
                return false;
            }
            try (java.io.FileInputStream inputStream = new java.io.FileInputStream(pfd.getFileDescriptor())) {
                inputStream.getChannel().position(0);
                return true;
            }
        } catch (Exception e) {
            SafeLogs.d(TAG, "canSeekUri()", "seek check failed: " + e.getMessage());
            return false;
        }
    }

    @NonNull
    private String appendRetJsonParam(@NonNull String uploadUrl) {
        if (uploadUrl.contains("ret-json=")) {
            return uploadUrl;
        }
        if (uploadUrl.contains("?")) {
            return uploadUrl + "&ret-json=1";
        }
        return uploadUrl + "?ret-json=1";
    }

    private void uploadSingleChunk(Account account, long createdTime, long offset, long chunkSize, long totalSize, long configuredChunkSize,
                                   boolean chunkedMode, boolean markSuccess, @Nullable File uploadFile,
                                   @Nullable Uri uploadUri, boolean uploadFromUri,
                                   int chunkNumber, int totalChunks, @NonNull String uploadUrl) throws SeafException {
        String mimeType = resolveMimeType(uploadFile, uploadUri, uploadFromUri);
        String resumableIdentifier = buildResumableIdentifier(currentTransferModel.file_name, totalSize);
        String resumableRelativePath = currentTransferModel.file_name;

        SafeLogs.d(TAG, "uploadSingleChunk()", "start transfer chunk, offset: " + offset + " remote path: " + currentTransferModel.target_path);
        RequestBody requestBody = buildUploadRequestBody(
                createdTime,
                offset,
                chunkSize,
                totalSize,
                configuredChunkSize,
                chunkedMode,
                uploadFile,
                uploadUri,
                uploadFromUri,
                chunkNumber,
                totalChunks,
                mimeType,
                resumableIdentifier,
                resumableRelativePath
        );
        Request request = buildUploadRequest(uploadUrl, requestBody, chunkedMode, offset, chunkSize, totalSize);
        executeUploadRequest(account, request, chunkedMode, markSuccess);
    }

    private RequestBody buildUploadRequestBody(long createdTime, long offset, long chunkSize, long totalSize, long configuredChunkSize,
                                               boolean chunkedMode, @Nullable File uploadFile, @Nullable Uri uploadUri,
                                               boolean uploadFromUri, int chunkNumber, int totalChunks,
                                               @NonNull String mimeType, @NonNull String resumableIdentifier,
                                               @NonNull String resumableRelativePath) throws SeafException {
        MultipartBody.Builder builder = new MultipartBody.Builder();
        builder.setType(MultipartBody.FORM);

        attachFilePart(builder, uploadFromUri, uploadUri, uploadFile, chunkedMode, offset, chunkSize, totalSize);
        appendTargetPathParams(builder);

        if (chunkedMode) {
            appendResumableParams(builder, chunkNumber, configuredChunkSize, chunkSize, totalSize,
                    mimeType, resumableIdentifier, resumableRelativePath, totalChunks);
        }

        if (createdTime != -1) {
            String cTime = Times.convertLong2Time(createdTime);
            SafeLogs.d(TAG, "file create time: " + cTime);
            builder.addFormDataPart("last_modify", cTime);
        }
        return builder.build();
    }

    private void attachFilePart(@NonNull MultipartBody.Builder builder, boolean uploadFromUri, @Nullable Uri uploadUri,
                                @Nullable File uploadFile, boolean chunkedMode, long offset, long chunkSize,
                                long totalSize) throws SeafException {
        if (uploadFromUri && uploadUri != null) {
            if (chunkedMode) {
                uriChunkRequestBody = new UriChunkRequestBody(getContext(), uploadUri, offset, chunkSize, totalSize, _fileTransferProgressListener);
                builder.addFormDataPart("file", currentTransferModel.file_name, uriChunkRequestBody);
            } else {
                uriRequestBody = new UriStreamRequestBody(getContext(), uploadUri, totalSize, _fileTransferProgressListener);
                builder.addFormDataPart("file", currentTransferModel.file_name, uriRequestBody);
            }
            return;
        }

        if (uploadFile == null || !uploadFile.exists()) {
            throw SeafException.NOT_FOUND_EXCEPTION;
        }
        if (chunkedMode) {
            fileChunkRequestBody = new FileChunkRequestBody(uploadFile, offset, chunkSize, _fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferModel.file_name, fileChunkRequestBody);
        } else {
            fileRequestBody = new FileStreamRequestBody(uploadFile, offset, chunkSize, _fileTransferProgressListener);
            builder.addFormDataPart("file", currentTransferModel.file_name, fileRequestBody);
        }
    }

    private void appendTargetPathParams(@NonNull MultipartBody.Builder builder) {
        if (currentTransferModel.transfer_strategy == ExistingFileStrategy.REPLACE) {
            builder.addFormDataPart("target_file", currentTransferModel.target_path);
            return;
        }
        builder.addFormDataPart("parent_dir", "/");
        String dir = currentTransferModel.getParentPath();
        dir = StringUtils.removeStart(dir, "/");
        builder.addFormDataPart("relative_path", dir);
    }

    private void appendResumableParams(@NonNull MultipartBody.Builder builder, int chunkNumber, long configuredChunkSize,
                                       long currentChunkSize, long totalSize, @NonNull String mimeType,
                                       @NonNull String resumableIdentifier, @NonNull String resumableRelativePath,
                                       int totalChunks) {
        builder.addFormDataPart("resumableChunkNumber", String.valueOf(chunkNumber));
        builder.addFormDataPart("resumableChunkSize", String.valueOf(configuredChunkSize));
        builder.addFormDataPart("resumableCurrentChunkSize", String.valueOf(currentChunkSize));
        builder.addFormDataPart("resumableTotalSize", String.valueOf(totalSize));
        builder.addFormDataPart("resumableType", mimeType);
        builder.addFormDataPart("resumableIdentifier", resumableIdentifier);
        builder.addFormDataPart("resumableFilename", currentTransferModel.file_name);
        builder.addFormDataPart("resumableRelativePath", resumableRelativePath);
        builder.addFormDataPart("resumableTotalChunks", String.valueOf(totalChunks));
    }

    private Request buildUploadRequest(@NonNull String uploadUrl, @NonNull RequestBody requestBody, boolean chunkedMode,
                                       long offset, long chunkSize, long totalSize) {
        Request.Builder requestBuilder = new Request.Builder()
                .url(uploadUrl)
                .post(requestBody)
                .addHeader("Connection", "keep-alive")
                .addHeader("User-Agent", Constants.UA.SEAFILE_ANDROID_UA)
                .addHeader("Content-Disposition", "attachment; filename=\"" + currentTransferModel.file_name + "\"");

        if (chunkedMode && chunkSize > 0 && totalSize > 0) {
            requestBuilder.addHeader("Content-Range", "bytes " + offset + "-" + (offset + chunkSize - 1) + "/" + totalSize);
        }
        Request request = requestBuilder.build();
        SafeLogs.d(TAG, "upload content-Type: " + requestBody.contentType());
        SafeLogs.d(TAG, "upload url: " + uploadUrl);
        SafeLogs.d(TAG, "upload headers: " + request.headers());
        return request;
    }

    private void executeUploadRequest(@NonNull Account account, @NonNull Request request, boolean chunkedMode, boolean markSuccess) throws SeafException {
        OkHttpClient client = chunkedMode ? getChunkUploadHttpClient(account) : getPrimaryHttpClient(account);
        newCall = client.newCall(request);
        try (Response response = newCall.execute()) {
            Protocol protocol = response.protocol();
            SafeLogs.d(TAG, "onRes()", "response code: " + response.code() + ", protocol: " + protocol);
            String fileId = parseUploadResponse(response, chunkedMode, markSuccess);
            if (markSuccess) {
                updateToSuccess(fileId);
            }
        } catch (Exception e) {
            SafeLogs.e(TAG, "upload file failed.");
            SafeLogs.e(e);
            throw ExceptionUtils.parseByThrowable(e);
        } finally {
            if (newCall != null) {
                SafeLogs.d(TAG, "transferFile()", "reset newCall object");
                newCall.cancel();
                newCall = null;
            }
            if (currentTransferModel.hasExtraMotionPhoto() && markSuccess) {
                com.blankj.utilcode.util.FileUtils.delete(currentTransferModel.motion_photo_path);
            }
        }
    }

    @NonNull
    private String resolveMimeType(@Nullable File uploadFile, @Nullable Uri uploadUri, boolean uploadFromUri) {
        try {
            if (uploadFromUri && uploadUri != null) {
                String mime = Utils.getMimeType(getContext(), uploadUri);
                if (!TextUtils.isEmpty(mime)) {
                    return mime;
                }
            } else if (uploadFile != null) {
                String mime = FileUtils.getMimeType(uploadFile);
                if (!TextUtils.isEmpty(mime)) {
                    return mime;
                }
            }
        } catch (Exception ignored) {
        }
        return Utils.MIME_APPLICATION_OCTET_STREAM;
    }

    @NonNull
    private String buildResumableIdentifier(@NonNull String fileName, long totalSize) {
        String seed = totalSize + "_" + fileName;
        String digest = EncryptUtils.encryptMD5ToString(seed);
        if (TextUtils.isEmpty(digest)) {
            return seed;
        }
        return digest + fileName;
    }

    /**
     * jpeg to heic
     */
    @Nullable
    private String convertJpegToHeicIfMotionPhoto() {
        try {
            boolean isJpeg = Utils.isJpeg(currentTransferModel.file_name);
            if (!isJpeg) {
                return null;
            }

            MotionPhotoDescriptor descriptor = null;
            if (currentTransferModel.full_path.startsWith("content://")) {
                descriptor = MotionPhotoDetector.parseJpegXmpWithUri(SeadroidApplication.getAppContext(), Uri.parse(currentTransferModel.full_path), true);

                if (descriptor.isMotionPhoto()) {
                    currentTransferModel.motion_photo_path = descriptor.tempJpegPath;
                }
            } else {
                descriptor = MotionPhotoDetector.parseJpegXmpWithFile(currentTransferModel.full_path);
                if (descriptor.isMotionPhoto()) {
                    // Prepare the path(full_path,not tempJpegPath) to the convert to HEIC
                    currentTransferModel.motion_photo_path = currentTransferModel.full_path;
                }
            }

            SafeLogs.e(TAG, descriptor.toString());

            if (!descriptor.isMotionPhoto()) {
                return null;
            }


            File tempFile = DataManager.createTempFile("tmp-hmp-", ".heic");

            // it is an error data
            if (descriptor.items.size() == 1) {
                return null;
            }

            // Semantic: Primary/GainMap/MotionPhoto
            int primaryIndex = getSpecialSemanticPosition(descriptor.items, Constants.MotionPhoto.PRIMARY);
            int videoIndex = getSpecialSemanticPosition(descriptor.items, Constants.MotionPhoto.MOTION_PHOTO);

            if (primaryIndex == -1 || videoIndex == -1) {
                return null;
            }

            String outHeicPath = HeicNative.ConvertJpeg2Heic(currentTransferModel.motion_photo_path, tempFile.getAbsolutePath());
            if (TextUtils.isEmpty(outHeicPath)) {
                SafeLogs.e(TAG, "convertJpegToHeicIfMotionPhoto()", "convertJpegToHeicIfMotionPhoto failed");
                return null;
            }
            return outHeicPath;
        } catch (Exception e) {
            SafeLogs.e(e);
        }
        return null;
    }

    public static int getSpecialSemanticPosition(List<MotionPhotoDescriptor.MotionPhotoItem> items, String semantic) {
        for (int i = 0; i < items.size(); i++) {
            String s = items.get(i).semantic;
            if (TextUtils.equals(semantic, s)) {
                return i;
            }
        }

        return -1;
    }

    private String parseUploadResponse(Response response, boolean chunkedMode, boolean markSuccess) throws SeafException, IOException {
        //req headers log
        Headers reqHeaders = response.request().headers();
        for (int i = 0; i < reqHeaders.size(); i++) {
            SafeLogs.d(TAG, "req-header: " + reqHeaders.name(i) + ": " + reqHeaders.value(i));
        }

        //res headers log
        Headers resHeaders = response.headers();
        for (int i = 0; i < resHeaders.size(); i++) {
            SafeLogs.d(TAG, "res-header: " + resHeaders.name(i) + ": " + resHeaders.value(i));
        }

        int code = response.code();
        try (ResponseBody body = response.body()) {
            if (body == null) {
                SafeLogs.d(TAG, "transferFile()", "body is null");

                if (response.isSuccessful()) {
                    return null;
                } else {
                    throw ExceptionUtils.parseHttpException(code, null);
                }
            }

            String bodyStr = body.string();
            if (response.isSuccessful()) {
                if (TextUtils.isEmpty(bodyStr)) {
                    return null;
                }

                // stream mode: response body is quoted file id string, e.g. "\"xxxxxxxx\""
                if (!chunkedMode) {
                    String fileId = bodyStr.trim().replace("\"", "");
                    SafeLogs.d(TAG, "transferFile()", "result body: " + bodyStr);
                    SafeLogs.d(TAG, "transferFile()", "parsed file ID: " + fileId);
                    return fileId;
                }

                // chunk mode and not final chunk: usually {"success": true}, no file id is expected
                if (!markSuccess) {
                    SafeLogs.d(TAG, "transferFile()", "chunk response body: " + bodyStr);
                    return null;
                }

                // chunk mode final chunk: response is array with first element containing id
                try {
                    JSONArray array = new JSONArray(bodyStr);
                    if (array.length() > 0) {
                        JSONObject first = array.optJSONObject(0);
                        if (first != null) {
                            String id = first.optString("id");
                            if (!TextUtils.isEmpty(id)) {
                                SafeLogs.d(TAG, "transferFile()", "chunk final id: " + id);
                                return id;
                            }
                        }
                    }
                    SafeLogs.d(TAG, "transferFile()", "chunk final response has no id, body: " + bodyStr);
                    return null;
                } catch (JSONException e) {
                    SafeLogs.d(TAG, "transferFile()", "invalid chunk final response, body: " + bodyStr);
                    return null;
                }
            } else {
                throw ExceptionUtils.parseHttpException(code, bodyStr);
            }
        }
    }

    @NonNull
    private String getFileUploadUrl(Account account, String repoId, String target_dir,
                                    boolean isUpdate) throws SeafException {
        SafeLogs.d(TAG, "getFileUploadUrl()", "target_dir: " + target_dir, "isUpdate: " + isUpdate);

        retrofit2.Response<String> res;
        try {
            if (isUpdate) {
                res = HttpManager.getHttpWithAccount(account)
                        .execute(FileService.class)
                        .getFileUpdateLink(repoId)
                        .execute();
            } else {
                res = HttpManager.getHttpWithAccount(account)
                        .execute(FileService.class)
                        .getFileUploadLink(repoId, "/")
                        .execute();
            }
        } catch (Exception e) {
            SafeLogs.e(TAG, "getFileUploadUrl", e.getMessage());
            throw ExceptionUtils.parseByThrowable(e);
        }

        if (!res.isSuccessful()) {
            SafeLogs.e(TAG, "getFileUploadUrl()", "response is not successful");
            try (ResponseBody errBody = res.errorBody()) {
                if (errBody != null) {
                    String msg = errBody.string();
                    throw ExceptionUtils.parseHttpException(res.code(), msg);
                }
            } catch (IOException e) {
                throw ExceptionUtils.parseHttpException(res.code(), null);
            }
        }

        String urlStr = res.body();
        urlStr = StringUtils.replace(urlStr, "\"", "");

        if (TextUtils.isEmpty(urlStr)) {
            SafeLogs.e(TAG, "getFileUploadUrl()", "urlStr is empty");
            throw SeafException.REQUEST_URL_EXCEPTION;
        }

        return urlStr;
    }

    public void updateToFailed(String transferResult) {
        currentTransferModel.transferred_size = 0L;
        currentTransferModel.transfer_status = TransferStatus.FAILED;
        currentTransferModel.err_msg = transferResult;
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);
    }

    private void updateToSuccess(String fileId) {
        currentTransferModel.transferred_size = currentTransferModel.file_size;
        currentTransferModel.transfer_status = TransferStatus.SUCCEEDED;
        currentTransferModel.err_msg = TransferResult.TRANSMITTED.name();
        GlobalTransferCacheList.updateTransferModel(currentTransferModel);

        if (currentTransferModel.save_to == SaveTo.DB) {
            if (currentTransferModel.data_source == FeatureDataSource.AUTO_UPDATE_LOCAL_FILE) {
                FileCacheStatusEntity transferEntity = FileCacheStatusEntity.convertFromUpload(currentTransferModel, fileId);
                AppDatabase.getInstance().fileCacheStatusDAO().insert(transferEntity);

                //
                AppDatabase.getInstance().direntDao().updateFileIdByPath(transferEntity.repo_id, transferEntity.full_path, fileId);
            } else {
                FileBackupStatusEntity transferEntity = FileBackupStatusEntity.convertTransferModel2This(currentTransferModel, fileId);
                AppDatabase.getInstance().fileTransferDAO().insert(transferEntity);
            }
        }
    }

    public boolean isInterrupt(SeafException result) {
        return result.equals(SeafException.OUT_OF_QUOTA) ||
                result.equals(SeafException.INVALID_PASSWORD) ||
                result.equals(SeafException.NETWORK_SSL_EXCEPTION) ||
                result.equals(SeafException.UNAUTHORIZED_EXCEPTION) ||
                result.equals(SeafException.NOT_FOUND_USER_EXCEPTION) ||
                result.equals(SeafException.SERVER_INTERNAL_ERROR) ||
                result.equals(SeafException.USER_CANCELLED_EXCEPTION);
    }

}
