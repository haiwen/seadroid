package com.seafile.seadroid2.worker.body;

import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.worker.TransferWorker;

import java.io.File;
import java.io.IOException;

import okhttp3.MediaType;
import okhttp3.RequestBody;
import okhttp3.internal.Util;
import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Source;

public class ProgressRequestBody extends RequestBody {

    private final File file;
    private final MediaType mediaType;
    FileTransferProgressListener fileTransferProgressListener;

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

//    @Override
//    public void writeTo(BufferedSink sink) throws IOException {
//        long fileLength = file.length();
//
//        long bytesWritten = 0L;
//        Source source = null;
//        try {
//            source = Okio.source(file);
//            Buffer buffer = new Buffer();
//            while (true) {
//                long bytesRead = source.read(buffer, TransferWorker.SEGMENT_SIZE);
//                if (bytesRead == -1) {
//                    break;
//                }
//
//                sink.write(buffer, bytesRead);
//                bytesWritten += bytesRead;
//
//                fileTransferProgressListener.onProgressNotify(bytesWritten, fileLength);
//            }
//
//        } finally {
//            Util.closeQuietly(source);
//        }
//    }

    public long temp = System.currentTimeMillis();

    @Override
    public void writeTo(BufferedSink sink) throws IOException {
        Source source;
        try {
            source = Okio.source(file);
            Buffer buf = new Buffer();
            // long remaining = contentLength();
            long current = 0;
            for (long readCount; (readCount = source.read(buf, 2048)) != -1; ) {
                sink.write(buf, readCount);
                current += readCount;
                long nowt = System.currentTimeMillis();
                // 1s refresh progress
                if (nowt - temp >= 1000) {
                    temp = nowt;
                    fileTransferProgressListener.onProgressNotify(current, file.length());
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
