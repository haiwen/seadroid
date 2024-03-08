package com.seafile.seadroid2.listener;

public interface ProgressListener {
    void onProgressNotify(String fileName, long cur, long total);

    boolean isCancelled();
}
