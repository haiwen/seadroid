package com.horizonbase.seadroid2.data;

public interface ProgressMonitor {
    void onProgressNotify(long total);

    boolean isCancelled();
}
