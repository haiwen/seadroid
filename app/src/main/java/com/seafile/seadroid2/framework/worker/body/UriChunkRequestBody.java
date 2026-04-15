package com.seafile.seadroid2.framework.worker.body;

import android.content.Context;
import android.net.Uri;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.service.ParentEventUploader;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.listener.FileTransferProgressListener;

import java.io.IOException;

import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Source;

/**
 * RequestBody for sliced upload from URI source.
 */
public class UriChunkRequestBody extends BaseRequestBody {
    private final Uri uri;
    private final Context context;
    private final long totalFileLength;
    private final long offset;
    private final long chunkSize;

    public UriChunkRequestBody(Context context,
                               Uri uri,
                               long offset,
                               long chunkSize,
                               long totalFileLength,
                               FileTransferProgressListener fileTransferProgressListener) {
        super(fileTransferProgressListener);
        this.context = context;
        this.uri = uri;
        this.offset = offset;
        this.chunkSize = chunkSize;
        this.totalFileLength = totalFileLength;
    }

    @Override
    public long contentLength() {
        return Math.max(chunkSize, 0L);
    }

    @Override
    public void writeTo(@NonNull BufferedSink sink) throws IOException {
        try (android.os.ParcelFileDescriptor pfd = context.getContentResolver().openFileDescriptor(uri, "r")) {
            if (pfd == null) {
                throw new IOException("Failed to open file descriptor for URI: " + uri);
            }

            try (java.io.FileInputStream inputStream = new java.io.FileInputStream(pfd.getFileDescriptor())) {
                if (offset > 0) {
                    inputStream.getChannel().position(offset);
                }

                try (Source source = Okio.source(inputStream); Buffer buffer = new Buffer()) {
                    long lastUpdateTime = System.currentTimeMillis();
                    long totalReadForChunk = 0;

                    SafeLogs.d(ParentEventUploader.TAG, "UriChunkRequestBody", "Chunk Upload","offset = " + offset);

                    while (totalReadForChunk < chunkSize) {
                        if (shouldStopUpload()) {
                            return;
                        }

                        long toRead = Math.min(TransferWorker.SEGMENT_SIZE, chunkSize - totalReadForChunk);
                        if (toRead <= 0) {
                            break;
                        }
                        long readCount = source.read(buffer, toRead);
                        if (readCount == -1) break;

                        sink.write(buffer, readCount);

                        totalReadForChunk += readCount;

                        long now = System.currentTimeMillis();
                        if (now - lastUpdateTime >= UPDATE_INTERVAL_MS) {
                            dispatchProgress(offset + totalReadForChunk, totalFileLength);
                            lastUpdateTime = now;
                        }
                    }

                    dispatchProgress(offset + totalReadForChunk, totalFileLength);
                }
            }
        } catch (IOException e) {
            SLogs.e(e);
            throw e;
        }
    }
}
