package com.seafile.seadroid2.data;

public interface ProgressMonitor {
    void onProgressNotify(long total, boolean updateTotal);

    boolean isCancelled();
}
