package com.seafile.seadroid2.ui.media.image;

import androidx.media3.common.util.UnstableApi;
import androidx.media3.datasource.DataSource;

import java.io.File;

@UnstableApi
public class FileRangeDataSourceFactory implements DataSource.Factory {
    private final File file;
    private final long start;
    private final long length;

    public FileRangeDataSourceFactory(File file, long start, long length) {
        this.file = file;
        this.start = start;
        this.length = length;
    }

    @Override
    public DataSource createDataSource() {
        return new FileRangeDataSource(file, start, length);
    }
}
