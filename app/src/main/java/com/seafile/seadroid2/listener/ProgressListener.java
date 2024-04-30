package com.seafile.seadroid2.listener;

public interface ProgressListener {
    void onProgress(String fileName, long cur, long total);

    boolean isCancelled();
}
