package com.seafile.seadroid2.ui.media.data_source;

import android.net.Uri;

import androidx.media3.common.C;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DataSpec;
import androidx.media3.datasource.TransferListener;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class MotionPhotoDataSource implements DataSource {
    private final byte[] data;
    private int position = 0;

    public MotionPhotoDataSource(byte[] data) {
        this.data = data;
    }

    @Override
    public void addTransferListener(TransferListener transferListener) {

    }

    @Override
    public long open(DataSpec dataSpec) throws IOException {
        position = (int) dataSpec.position;
        return data.length - position;
    }

    @Override
    public void close() throws IOException {

    }

    @Override
    public Map<String, List<String>> getResponseHeaders() {
        return Collections.emptyMap();
    }

    @Override
    public int read(byte[] buffer, int offset, int readLength) throws IOException {
        if (position >= data.length) {
            return C.RESULT_END_OF_INPUT;
        }
        int bytesToRead = Math.min(readLength, data.length - position);
        System.arraycopy(data, position, buffer, offset, bytesToRead);
        position += bytesToRead;
        return bytesToRead;
    }

    @Override
    public Uri getUri() {
        return Uri.EMPTY;
    }
}
