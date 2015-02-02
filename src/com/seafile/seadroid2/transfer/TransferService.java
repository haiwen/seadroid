package com.seafile.seadroid2.transfer;

import java.io.File;
import java.util.List;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.transfer.TransferManager.TransferListener;
import com.seafile.seadroid2.util.Utils;

public class TransferService extends Service implements TransferListener {
    public static final String BROADCAST_ACTION = "com.seafile.seadroid.TX_BROADCAST";
    public static final String BROADCAST_FILE_DOWNLOAD_SUCCESS = "downloaded";
    public static final String BROADCAST_FILE_DOWNLOAD_FAILED = "downloadFailed";
    public static final String BROADCAST_FILE_DOWNLOAD_PROGRESS = "downloadProgress";

    public static final String BROADCAST_FILE_UPLOAD_SUCCESS = "uploaded";
    public static final String BROADCAST_FILE_UPLOAD_FAILED = "uploadFailed";
    public static final String BROADCAST_FILE_UPLOAD_PROGRESS = "uploadProgress";
    public static final String BROADCAST_FILE_UPLOAD_CANCELLED = "uploadCancelled";

    private static final String DEBUG_TAG = "TransferService";

    private final IBinder mBinder = new TransferBinder();
    private TransferManager txManager;

    @Override
    public void onCreate() {
        txManager = new TransferManager();
        txManager.setListener(this);

    }

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        txManager.unsetListener();

    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    public class TransferBinder extends Binder {
        public TransferService getService() {
            return TransferService.this;
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        // Log.d(DEBUG_TAG, "onBind");
        return mBinder;
    }
    
    /**
     * call this method to handle upload request, like file upload or camera upload.
     *  
     * Note: use isCopyToLocal to mark automatic camera upload if true, or file upload if false.
     * @param account
     * @param repoID
     * @param repoName
     * @param dir
     * @param filePath
     * @param isUpdate
     * @param isCopyToLocal
     * @return
     */
    public int addUploadTask(Account account, String repoID, String repoName, String dir,
            String filePath, boolean isUpdate, boolean isCopyToLocal) {
        return addTaskToUploadQue(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal);
    }

    public int addTaskToUploadQue(Account account, String repoID, String repoName, String dir,
                             String filePath, boolean isUpdate, boolean isCopyToLocal) {
        return txManager.addTaskToUploadQue(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal);
    }

    public int addDownloadTask(Account account, String repoName, String repoID, String path) {
        return addTaskToDownloadQue(account, repoName, repoID, path);
    }

    public int addTaskToDownloadQue(Account account, String repoName, String repoID, String path) {
        return txManager.addTaskToDownloadQue(account, repoName, repoID, path);
    }

    public boolean isDownloading() {
        return txManager.isDownloading();
    }

    public boolean isUploading() {
        return txManager.isUploading();
    }

    public UploadTaskInfo getUploadTaskInfo(int taskID) {
        return txManager.getUploadTaskInfo(taskID);
    }

    public List<UploadTaskInfo> getAllUploadTaskInfos() {
        return txManager.getAllUploadTaskInfos();
    }
    
    public List<DownloadTaskInfo> getAllDownloadTaskInfos() {
        return txManager.getAllDownloadTaskInfos();
    }

    public List<DownloadTaskInfo> getDownloadTaskInfosByPath(String repoID, String dir) {
        return txManager.getDownloadTaskInfosByPath(repoID, dir);
    }

    public void removeUploadTask(int taskID) {
        txManager.removeUploadTask(taskID);
    }

    public void removeDownloadTask(int taskID) {
        txManager.removeDownloadTask(taskID);
    }

    public void removeAllDownloadTasksByState(TransferManager.TaskState taskState) {
        txManager.removeAllDownloadTasksByState(taskState);

    }

    public void removeAllUploadTasksByState(TransferManager.TaskState taskState) {
        txManager.removeAllUploadTasksByState(taskState);

    }

    public void removeFinishedUploadTasks() {
        txManager.removeFinishedUploadTasks();
    }

    public void cancelUploadTask(int taskID) {
        txManager.cancelUploadTask(taskID);
    }

    public void cancelUploadTaskInQue(int taskID) {
        cancelUploadTask(taskID);
        txManager.removeCancelledUploadItemInQue(taskID);
        txManager.uploadNext();
    }

    public void cancelAllUploadTasks() {
        List<UploadTaskInfo> uploadTaskInfos = txManager.getAllUploadTaskInfos();
        for (TransferTaskInfo uploadTaskInfo : uploadTaskInfos) {
            cancelUploadTask(uploadTaskInfo.taskID);
        }

    }

    public void cancelAllCameraUploadTasks() {
        List<UploadTaskInfo> uploadTaskInfos = getAllUploadTaskInfos();
        for (UploadTaskInfo uploadTaskInfo : uploadTaskInfos) {
            // use isCopyToLocal as a flag to mark a camera photo upload task if false
            // mark a file upload task if true
            if (!uploadTaskInfo.isCopyToLocal) {
                cancelUploadTask(uploadTaskInfo.taskID);
            }
        }
    }

    public void retryUploadTask(int taskID) {
        txManager.retryUploadTask(taskID);
    }

    public void retryDownloadTask(int taskID) {
        txManager.retryDownloadTask(taskID);
    }

    public DownloadTaskInfo getDownloadTaskInfo(int taskID) {
        return txManager.getDownloadTaskInfo(taskID);
    }

    @Override
    public void onFileUploadProgress(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_PROGRESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploaded(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_SUCCESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadCancelled(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_CANCELLED).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadFailed(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_FAILED).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloadProgress(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_DOWNLOAD_PROGRESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloaded(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_DOWNLOAD_SUCCESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloadFailed(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_DOWNLOAD_FAILED).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    public void cancelDownloadTask(int taskID) {
        txManager.cancelDownloadTask(taskID);
    }

    public void cancelDownloadTaskInQue(int taskID) {
        cancelDownloadTask(taskID);
        txManager.removeCancelledDownloadItemInQue(taskID);
        txManager.downloadNext();
    }

    public void cancellAllDownloadTasks() {
        List<DownloadTaskInfo> downloadTaskInfos = txManager.getAllDownloadTaskInfos();
        for (DownloadTaskInfo downloadTaskInfo : downloadTaskInfos) {
            cancelDownloadTask(downloadTaskInfo.taskID);
        }
    }

}

interface TransferDBHelper {
    void saveUploadTaskInfo();

    void removeUploadTaskInfo();

    List<UploadTaskInfo> getUploadTaskInfoList();
}

interface UpdateTaskListener {
    void onTaskSuccess(UploadTaskInfo info);

    void onTaskFailed(UploadTaskInfo info);
}

/**
 * Retries to auto update changed files util the update succeeds.
 */
class PersistentTransferScheduler implements UpdateTaskListener {
    TransferDBHelper helper;
    TransferService service;

    public void addPersistentUpdateTask() {
    }

    @Override
    public void onTaskFailed(UploadTaskInfo info) {
    }

    @Override
    public void onTaskSuccess(UploadTaskInfo info) {
    }

    public void callback() {
        if (!Utils.isNetworkOn()) {
            return;
        }

        for (UploadTaskInfo info : helper.getUploadTaskInfoList()) {
            Account account = null;
            File file = new File(info.localFilePath);
            if (!file.exists()) {
                continue;
            }

            service.addUploadTask(account, info.repoID, info.repoName, info.parentDir,
                    info.localFilePath, true, true);
        }
    }
}
