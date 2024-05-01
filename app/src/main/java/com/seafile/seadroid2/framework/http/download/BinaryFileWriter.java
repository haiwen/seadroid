package com.seafile.seadroid2.framework.http.download;

import com.seafile.seadroid2.framework.http.callback.ProgressCallback;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class BinaryFileWriter implements AutoCloseable {
    private static final int CHUNK_SIZE = 4096;
    private final OutputStream outputStream;
    private final ProgressCallback callback;

    public BinaryFileWriter(OutputStream outputStream, ProgressCallback callback) {
        this.outputStream = outputStream;
        this.callback = callback;
    }

    private long temp = System.currentTimeMillis();

    public long write(InputStream inputStream, long totalSize) throws IOException {
        try (BufferedInputStream input = new BufferedInputStream(inputStream)) {
            byte[] dataBuffer = new byte[CHUNK_SIZE];
            int readBytes;
            long totalBytes = 0;
            while ((readBytes = input.read(dataBuffer)) != -1) {
                totalBytes += readBytes;
                outputStream.write(dataBuffer, 0, readBytes);

                long now = System.currentTimeMillis();
                if (now - temp > 250) {
                    temp = now;
                    //
                    callback.onProgress(totalBytes, totalSize);
                }

            }
            return totalBytes;
        }
    }

    @Override
    public void close() throws Exception {
        outputStream.close();
    }
}
