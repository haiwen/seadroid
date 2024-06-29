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

    public long temp = System.currentTimeMillis();

    @Override
    public void writeTo(@NonNull BufferedSink sink) throws IOException {
        try (Source source = Okio.source(file)) {
            Buffer buf = new Buffer();

            long fileLength = file.length();

            long current = 0;
            for (long readCount; (readCount = source.read(buf, TransferWorker.SEGMENT_SIZE)) != -1; ) {
                sink.write(buf, readCount);
                current += readCount;

                long nowt = System.currentTimeMillis();
                // 1s refresh progress
                if (nowt - temp >= 1000) {
                    temp = nowt;
                    if (fileTransferProgressListener != null) {
                        fileTransferProgressListener.onProgressNotify(current, fileLength);
                    }
                }
            }

            //notify complete
            if (fileTransferProgressListener != null) {
                fileTransferProgressListener.onProgressNotify(fileLength, fileLength);
            }
        }
    }
}
