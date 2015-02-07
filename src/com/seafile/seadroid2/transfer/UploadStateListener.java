package com.seafile.seadroid2.transfer;

/**
 * Upload state listener
 *
 * Created by Logan on 15/2/7.
 */
public interface UploadStateListener {
    void onFileUploadProgress(int taskID);
    void onFileUploaded(int taskID);
    void onFileUploadCancelled(int taskID);
    void onFileUploadFailed(int taskID);
}
