package com.seafile.seadroid2.framework.worker.body;

import android.content.Context;
import android.net.Uri;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.listener.FileTransferProgressListener;

import java.io.IOException;
import java.io.InputStream;

import okhttp3.MediaType;
import okhttp3.RequestBody;
import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Source;

public class ProgressUriRequestBody extends RequestBody {
    public static long UPDATE_INTERVAL_MS = 1000L;

    private final Uri uri;
    private final MediaType mediaType;
    private final FileTransferProgressListener fileTransferProgressListener;
    private final Context context;
    private final long estimationFileLength;
    private boolean isStop = false;

    public ProgressUriRequestBody(Context context, Uri uri, long size, FileTransferProgressListener fileTransferProgressListener) {
        this.context = context;
        this.uri = uri;
        this.mediaType = MediaType.parse("application/octet-stream");
        this.fileTransferProgressListener = fileTransferProgressListener;
        this.estimationFileLength = size;
    }

    public void setStop(boolean stop) {
        isStop = stop;
    }

    @Override
    public MediaType contentType() {
        return mediaType;
    }

    /**
     * maybe -1 (chunked)
     * @see com.seafile.seadroid2.framework.service.FileUploadUtils#resolveSize(Context, Uri)
     */
    @Override
    public long contentLength() {
        return estimationFileLength;
    }


    @Override
    public void writeTo(@NonNull BufferedSink sink) throws IOException {
        try (InputStream inputStream = context.getContentResolver().openInputStream(uri)) {
            if (inputStream == null) {
                throw new IOException("Failed to open input stream for URI: " + uri);
            }

            try (Source source = Okio.source(inputStream); Buffer buffer = new Buffer()) {
                long lastUpdateTime = System.currentTimeMillis();
                long bytesWrittenSinceUpdate = 0;

                while (true) {
                    if (Thread.currentThread().isInterrupted()) {
                        SLogs.e(new InterruptedException("Upload canceled"));
                        return;
                    }

                    if (isStop) {
                        return;
                    }

                    long readCount = source.read(buffer, TransferWorker.SEGMENT_SIZE);
                    if (readCount == -1) break; // End of file

                    sink.write(buffer, readCount);

                    bytesWrittenSinceUpdate += readCount;
                    // Throttle progress updates
                    long now = System.currentTimeMillis();
                    if (now - lastUpdateTime >= UPDATE_INTERVAL_MS) {
                        updateProgress(bytesWrittenSinceUpdate, estimationFileLength);
                        lastUpdateTime = now;
                    }
                }

                // Final update for completion to ensure 100% is reported
                // if the loop finishes before an interval update.
                updateProgress(estimationFileLength, estimationFileLength);
            }


        } catch (IOException e) {
            SLogs.e(e);
            throw e;
        }
    }

    private void updateProgress(long current, long total) {
        if (fileTransferProgressListener != null) {
            fileTransferProgressListener.onProgressNotify(current, total);
        }
    }
}
