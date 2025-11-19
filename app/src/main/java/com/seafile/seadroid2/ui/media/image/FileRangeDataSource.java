package com.seafile.seadroid2.ui.media.image;

import android.net.Uri;

import androidx.annotation.OptIn;
import androidx.media3.common.C;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DataSpec;
import androidx.media3.datasource.TransferListener;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;
import java.util.Map;

@OptIn(markerClass = UnstableApi.class)
public class FileRangeDataSource implements DataSource {
    private final File file;
    private final long start;
    private final long length;
    private RandomAccessFile raf;
    private long bytesRemaining;

    public FileRangeDataSource(File file, long start, long length) {
        this.file = file;
        this.start = start;
        this.length = length;
    }

    @UnstableApi
    @Override
    public void addTransferListener(TransferListener transferListener) {

    }

    @Override
    public long open(DataSpec dataSpec) throws IOException {
        raf = new RandomAccessFile(file, "r");

        long requestPosition = dataSpec.position;
        long absolutePosition = start + requestPosition; // 映射到 JPEG 内

        raf.seek(absolutePosition);

        if (dataSpec.length == C.LENGTH_UNSET) {
            bytesRemaining = length - requestPosition;
        } else {
            bytesRemaining = dataSpec.length;
        }

        return bytesRemaining;
    }

    @Override
    public int read(byte[] buffer, int offset, int readLength) throws IOException {
        if (bytesRemaining == 0) return C.RESULT_END_OF_INPUT;

        int toRead = (int) Math.min(readLength, bytesRemaining);
        int read = raf.read(buffer, offset, toRead);
        if (read == -1) return C.RESULT_END_OF_INPUT;

        bytesRemaining -= read;
        return read;
    }

    @Override
    public Uri getUri() {
        return Uri.fromFile(file);
    }

    @Override
    public Map<String, List<String>> getResponseHeaders() {
        return DataSource.super.getResponseHeaders();
    }

    @Override
    public void close() throws IOException {
        if (raf != null) raf.close();
        raf = null;
    }
}
