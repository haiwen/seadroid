package com.seafile.seadroid2.framework.worker.body;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.io.File;
import java.io.IOException;

import okhttp3.MediaType;
import okhttp3.RequestBody;
import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Source;

public class ProgressRequestBody extends RequestBody {

    private final File file;
    private final MediaType mediaType;
    private final FileTransferProgressListener fileTransferProgressListener;
    private final long offset;
    private final long chunkSize;
    private boolean isStop = false;// Flag to stop the upload

    public ProgressRequestBody(File file, FileTransferProgressListener fileTransferProgressListener) {
        this(file, 0, file.length(), fileTransferProgressListener);
    }

    public ProgressRequestBody(File file, long offset, long chunkSize, FileTransferProgressListener fileTransferProgressListener) {
        this.file = file;
        this.offset = offset;
        this.chunkSize = chunkSize;

        //Hardcoded MediaType to "application/octet-stream"
        this.mediaType = MediaType.parse("application/octet-stream");
        this.fileTransferProgressListener = fileTransferProgressListener;
    }

    public void setStop(boolean stop) {
        isStop = stop;
    }

    @Override
    public MediaType contentType() {
        return mediaType;
    }

    @Override
    public long contentLength() {
        return chunkSize;
    }

    @Override
    public void writeTo(@NonNull BufferedSink sink) throws IOException {
        try (java.io.RandomAccessFile randomAccessFile = new java.io.RandomAccessFile(file, "r")) {
            if (offset > 0) {
                randomAccessFile.seek(offset);
            }
            try (java.io.FileInputStream inputStream = new java.io.FileInputStream(randomAccessFile.getFD());
                 Source source = Okio.source(inputStream); Buffer buffer = new Buffer()) {
                long fileLength = file.length();
                long lastUpdateTime = System.currentTimeMillis();
                long totalReadForChunk = 0;

                while (totalReadForChunk < chunkSize) {
                    if (Thread.currentThread().isInterrupted()) {
                        SLogs.e(new InterruptedException("Upload canceled"));
                        return;
                    }

                    if (isStop) {
                        return;
                    }

                    long toRead = Math.min(TransferWorker.SEGMENT_SIZE, chunkSize - totalReadForChunk);
                    long readCount = source.read(buffer, toRead);
                    if (readCount == -1) break; // End of file

                    sink.write(buffer, readCount);

                    totalReadForChunk += readCount;
                    // Throttle progress updates
                    long now = System.currentTimeMillis();
                    if (now - lastUpdateTime >= ProgressUriRequestBody.UPDATE_INTERVAL_MS) {
                        updateProgress(offset + totalReadForChunk, fileLength);
                        lastUpdateTime = now;
                    }
                }

                // Final update for completion to ensure 100% is reported
                // if the loop finishes before an interval update.
                updateProgress(offset + totalReadForChunk, fileLength);
            }
        }
    }

    private void updateProgress(long current, long total) {
        if (fileTransferProgressListener != null) {
            fileTransferProgressListener.onProgressNotify(current, total);
        }
    }
}
