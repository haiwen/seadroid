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
    private boolean isStop = false;// Flag to stop the upload

    public ProgressRequestBody(File file, FileTransferProgressListener fileTransferProgressListener) {
        this.file = file;

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
        return file.length();
    }

    private static final int UPDATE_INTERVAL_MS = 1000;

    @Override
    public void writeTo(@NonNull BufferedSink sink) throws IOException {
        try (Source source = Okio.source(file); Buffer buffer = new Buffer()) {
            long fileLength = file.length();
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
                    updateProgress(bytesWrittenSinceUpdate, fileLength);
                    lastUpdateTime = now;
                }
            }

            // Final update for completion to ensure 100% is reported
            // if the loop finishes before an interval update.
            updateProgress(fileLength, fileLength);
        }
    }

    private void updateProgress(long current, long total) {
        if (fileTransferProgressListener != null) {
            fileTransferProgressListener.onProgressNotify(current, total);
        }
    }
}
