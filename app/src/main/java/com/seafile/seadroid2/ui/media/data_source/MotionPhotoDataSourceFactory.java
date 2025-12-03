package com.seafile.seadroid2.ui.media.data_source;

import androidx.media3.datasource.DataSource;

public class MotionPhotoDataSourceFactory implements DataSource.Factory {

    private byte[] bytes;

    public MotionPhotoDataSourceFactory(byte[] bytes) {
        this.bytes = bytes;
    }

    @Override
    public DataSource createDataSource() {
        return new MotionPhotoDataSource(bytes);
    }
}
