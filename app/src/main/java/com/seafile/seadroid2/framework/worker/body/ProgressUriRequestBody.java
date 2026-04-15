package com.seafile.seadroid2.framework.worker.body;

import android.content.Context;
import android.net.Uri;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.listener.FileTransferProgressListener;

import java.io.IOException;

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
    private final long offset;
    private final long chunkSize;
    private boolean isStop = false;

    public ProgressUriRequestBody(Context context, Uri uri, long size, FileTransferProgressListener fileTransferProgressListener) {
        this(context, uri, 0, size, size, fileTransferProgressListener);
    }

    public ProgressUriRequestBody(Context context, Uri uri, long offset, long chunkSize, long size, FileTransferProgressListener fileTransferProgressListener) {
        this.context = context;
        this.uri = uri;
        this.offset = offset;
        this.chunkSize = chunkSize;
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
        return chunkSize < 0 ? -1 : chunkSize;
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
                    boolean readToEnd = chunkSize < 0;

                    while (readToEnd || totalReadForChunk < chunkSize) {
                        if (Thread.currentThread().isInterrupted()) {
                            SLogs.e(new InterruptedException("Upload canceled"));
                            return;
                        }

                        if (isStop) {
                            return;
                        }

                        long toRead = readToEnd
                                ? TransferWorker.SEGMENT_SIZE
                                : Math.min(TransferWorker.SEGMENT_SIZE, chunkSize - totalReadForChunk);
                        long readCount = source.read(buffer, toRead);
                        if (readCount == -1) break; // End of file

                        sink.write(buffer, readCount);

                        totalReadForChunk += readCount;
                        // Throttle progress updates
                        long now = System.currentTimeMillis();
                        if (now - lastUpdateTime >= UPDATE_INTERVAL_MS) {
                            long safeTotal = estimationFileLength > 0 ? estimationFileLength : (offset + totalReadForChunk);
                            updateProgress(offset + totalReadForChunk, safeTotal);
                            lastUpdateTime = now;
                        }
                    }

                    // Final update for completion to ensure 100% is reported
                    // if the loop finishes before an interval update.
                    long safeTotal = estimationFileLength > 0 ? estimationFileLength : (offset + totalReadForChunk);
                    updateProgress(offset + totalReadForChunk, safeTotal);
                }
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
