package com.seafile.seadroid2.transfer;

/**
 * Download state listener
 *
 * Created by Logan on 15/2/7.
 */
public interface DownloadStateListener {
    void onFileDownloadProgress(int taskID);
    void onFileDownloaded(int taskID);
    void onFileDownloadFailed(int taskID);
}
