package com.seafile.seadroid2.framework.http.callback;

public interface ProgressCallback {
    void onProgress(long transferSize, long totalSize);
}
