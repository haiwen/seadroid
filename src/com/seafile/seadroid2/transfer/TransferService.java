package com.seafile.seadroid2.transfer;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.notification.UploadNotificationProvider;

import java.util.List;

public class TransferService extends Service {
    private static final String DEBUG_TAG = "TransferService";

    private final IBinder mBinder = new TransferBinder();

    public DownloadTaskManager getDownloadTaskManager() {
        return downloadTaskManager;
    }

    public UploadTaskManager getUploadTaskManager() {
        return uploadTaskManager;
    }

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

    public boolean isTransferring() {
        List<UploadTaskInfo> uInfos = getNoneCameraUploadTaskInfos();
        for (UploadTaskInfo info : uInfos) {
            if (info.state.equals(TaskState.INIT)
                    || info.state.equals(TaskState.TRANSFERRING))
                return true;
        }

        List<DownloadTaskInfo> dInfos = getAllDownloadTaskInfos();
        for (DownloadTaskInfo info : dInfos) {
            if (info.state.equals(TaskState.INIT)
                    || info.state.equals(TaskState.TRANSFERRING))
                return true;
        }

        return false;
    }

    @Override
    public IBinder onBind(Intent intent) {
        // Log.d(DEBUG_TAG, "onBind");
        return mBinder;
    }

    // -------------------------- upload task --------------------//
    public int addTaskToUploadQue(Account account, String repoID, String repoName, String dir,
                             String filePath, boolean isUpdate, boolean isCopyToLocal) {
        return uploadTaskManager.addTaskToQue(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal);
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

    public List<UploadTaskInfo> getNoneCameraUploadTaskInfos() {
        return uploadTaskManager.getNoneCameraUploadTaskInfos();
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
        uploadTaskManager.cancelAllUploadNotification();
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

    public int getDownloadingFileCountByPath(String repoID, String dirPath) {
        return downloadTaskManager.getDownloadingFileCountByPath(repoID, dirPath);
    }

    public List<DownloadTaskInfo> getDownloadTaskInfosByPath(String repoID, String dir) {
        return downloadTaskManager.getTaskInfoListByPath(repoID, dir);
    }

    public List<DownloadTaskInfo> getDownloadTaskInfosByRepo(String repoID) {
        return downloadTaskManager.getTaskInfoListByRepo(repoID);
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

    public void cancelNotification() {
        downloadTaskManager.cancelAllDownloadNotification();
    }

    public void cancelDownloadTaskInQue(int taskID) {
        downloadTaskManager.cancel(taskID);
        downloadTaskManager.doNext();
    }

    public void cancellAllDownloadTasks() {
        downloadTaskManager.cancelAll();
        downloadTaskManager.cancelAllDownloadNotification();
    }

    // -------------------------- upload notification --------------------//

    public void saveUploadNotifProvider(UploadNotificationProvider provider) {
        uploadTaskManager.saveUploadNotifProvider(provider);
    }

    public boolean hasUploadNotifProvider() {
        return uploadTaskManager.hasNotifProvider();
    }

    public UploadNotificationProvider getUploadNotifProvider() {
        return uploadTaskManager.getNotifProvider();
    }

    // -------------------------- download notification --------------------//

    public void saveDownloadNotifProvider(DownloadNotificationProvider provider) {
        downloadTaskManager.saveNotifProvider(provider);
    }

    public boolean hasDownloadNotifProvider() {
        return downloadTaskManager.hasNotifProvider();
    }

    public DownloadNotificationProvider getDownloadNotifProvider() {
        return downloadTaskManager.getNotifProvider();
    }

}
