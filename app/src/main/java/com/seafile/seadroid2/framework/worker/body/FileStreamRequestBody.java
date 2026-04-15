package com.seafile.seadroid2.framework.worker.body;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.io.File;
import java.io.IOException;

import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Source;

public class FileStreamRequestBody extends BaseRequestBody {

    private final File file;
    private final long offset;
    private final long chunkSize;

    public FileStreamRequestBody(File file, FileTransferProgressListener fileTransferProgressListener) {
        this(file, 0, file.length(), fileTransferProgressListener);
    }

    public FileStreamRequestBody(File file, long offset, long chunkSize, FileTransferProgressListener fileTransferProgressListener) {
        super(fileTransferProgressListener);
        this.file = file;
        this.offset = offset;
        this.chunkSize = chunkSize;
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
                    if (shouldStopUpload()) {
                        return;
                    }

                    long toRead = Math.min(TransferWorker.SEGMENT_SIZE, chunkSize - totalReadForChunk);
                    if (toRead <= 0) {
                        break;
                    }
                    long readCount = source.read(buffer, toRead);
                    if (readCount == -1) break; // End of file

                    sink.write(buffer, readCount);

                    totalReadForChunk += readCount;
                    // Throttle progress updates
                    long now = System.currentTimeMillis();
                    if (now - lastUpdateTime >= UPDATE_INTERVAL_MS) {
                        dispatchProgress(offset + totalReadForChunk, fileLength);
                        lastUpdateTime = now;
                    }
                }

                // Final update for completion to ensure 100% is reported
                // if the loop finishes before an interval update.
                dispatchProgress(offset + totalReadForChunk, fileLength);
            }
        }
    }
}
