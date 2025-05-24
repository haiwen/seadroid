package com.seafile.seadroid2.framework.worker.body;

import androidx.annotation.NonNull;

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

    public ProgressRequestBody(File file, FileTransferProgressListener fileTransferProgressListener) {
        this.file = file;
        this.mediaType = MediaType.parse("application/octet-stream");
        this.fileTransferProgressListener = fileTransferProgressListener;
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
                long readCount = source.read(buffer, TransferWorker.SEGMENT_SIZE);
                if (readCount == -1) break;

                sink.write(buffer, readCount);

                bytesWrittenSinceUpdate += readCount;
                // Throttle progress updates
                long now = System.currentTimeMillis();
                if (now - lastUpdateTime >= UPDATE_INTERVAL_MS) {
                    updateProgress(bytesWrittenSinceUpdate, fileLength);
                    lastUpdateTime = now;
                }
            }

            // Final update for completion
            updateProgress(fileLength, fileLength);
        }
    }

    private void updateProgress(long current, long total) {
        if (fileTransferProgressListener != null) {
            fileTransferProgressListener.onProgressNotify(current, total);
        }
    }
}
