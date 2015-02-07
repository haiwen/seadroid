package com.seafile.seadroid2.transfer;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;
import com.seafile.seadroid2.account.Account;

import java.util.List;

public class TransferService extends Service {
    private static final String DEBUG_TAG = "TransferService";

    private final IBinder mBinder = new TransferBinder();
    private DownloadTaskManager downloadTaskManager;
    private UploadTaskManager uploadTaskManager;

    @Override
    public void onCreate() {
        downloadTaskManager = new DownloadTaskManager();
        uploadTaskManager = new UploadTaskManager();
    }

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
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

    // -------------------------- upload task --------------------//
    public void addTaskToUploadQue(Account account, String repoID, String repoName, String dir,
                             String filePath, boolean isUpdate, boolean isCopyToLocal) {
        uploadTaskManager.addTaskToQue(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal);
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
    public void addUploadTask(Account account, String repoID, String repoName, String dir,
            String filePath, boolean isUpdate, boolean isCopyToLocal) {
        addTaskToUploadQue(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal);
    }

    public UploadTaskInfo getUploadTaskInfo(int taskID) {
        return (UploadTaskInfo) uploadTaskManager.getTaskInfo(taskID);
    }

    public List<UploadTaskInfo> getAllUploadTaskInfos() {
        return (List<UploadTaskInfo>) uploadTaskManager.getAllTaskInfoList();
    }

    public void removeAllUploadTasksByState(TaskState taskState) {
        uploadTaskManager.removeByState(taskState);

    }

    public void cancelUploadTaskInQue(int taskID) {
        uploadTaskManager.cancel(taskID);
        uploadTaskManager.doNext();
    }

    public void cancelAllUploadTasks() {
        uploadTaskManager.cancelAll();
    }

    public void cancelAllCameraUploadTasks() {
        uploadTaskManager.cancelAllCameraUploadTasks();
    }

    public void retryUploadTask(int taskID) {
        uploadTaskManager.retry(taskID);
    }

    public void removeUploadTask(int taskID) {
        uploadTaskManager.removeInAllTaskList(taskID);
    }

    // -------------------------- download task --------------------//
    public int addDownloadTask(Account account, String repoName, String repoID, String path) {
        return downloadTaskManager.addTask(account, repoName, repoID, path);
    }

    public void addTaskToDownloadQue(Account account, String repoName, String repoID, String path) {
        downloadTaskManager.addTaskToQue(account, repoName, repoID, path);
    }

    public List<DownloadTaskInfo> getAllDownloadTaskInfos() {
        return (List<DownloadTaskInfo>) downloadTaskManager.getAllTaskInfoList();
    }

    public int getDownloadingFileCountByPath(String repoID, String dir) {
        return downloadTaskManager.getDownloadingFileCountByPath(repoID, dir);
    }

    public List<DownloadTaskInfo> getDownloadTaskInfosByPath(String repoID, String dir) {
        return downloadTaskManager.getTaskInfoListByPath(repoID, dir);
    }

    public void removeDownloadTask(int taskID) {
        downloadTaskManager.removeInAllTaskList(taskID);
    }

    public void removeAllDownloadTasksByState(TaskState taskState) {
        downloadTaskManager.removeByState(taskState);

    }

    public void retryDownloadTask(int taskID) {
        downloadTaskManager.retry(taskID);
    }

    public DownloadTaskInfo getDownloadTaskInfo(int taskID) {
        return (DownloadTaskInfo) downloadTaskManager.getTaskInfo(taskID);
    }

    public void cancelDownloadTask(int taskID) {
        cancelDownloadTaskInQue(taskID);
    }

    public void cancelDownloadTaskInQue(int taskID) {
        downloadTaskManager.cancel(taskID);
        downloadTaskManager.doNext();
    }

    public void cancellAllDownloadTasks() {
        downloadTaskManager.cancelAll();
    }

}
