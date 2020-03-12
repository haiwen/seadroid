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

    /**
     * Call this method to handle upload request, like file upload or camera upload.
     * Uploading tasks are managed in a queue.
     *
     * Note: use isCopyToLocal to mark automatic camera upload if false, or file upload if true.
     * @param account
     * @param repoID
     * @param repoName
     * @param dir
     * @param filePath
     * @param isUpdate
     * @param isCopyToLocal
     * @return
     */
    public int addTaskToUploadQue(Account account, String repoID, String repoName, String dir, String filePath, boolean isUpdate, boolean isCopyToLocal) {
        return uploadTaskManager.addTaskToQue(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal,false);
    }

    /**
     * Call this method to handle upload request, like file upload or camera upload.
     * Uploading tasks are managed in a queue.
     *
     * Note: use isCopyToLocal to mark automatic camera upload if false, or file upload if true.
     * @param account
     * @param repoID
     * @param repoName
     * @param dir
     * @param filePath
     * @param isUpdate
     * @param isCopyToLocal
     * @param version
     * @return
     */
    public int addTaskToUploadQueBlock(Account account, String repoID, String repoName, String dir,
                                       String filePath, boolean isUpdate, boolean isCopyToLocal) {
        return uploadTaskManager.addTaskToQue(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal, true);
    }

    /**
     * Call this method to handle upload request, like file upload or camera upload.
     *
     * Note: use isCopyToLocal to mark automatic camera upload if false, or file upload if true.
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

    public void restartAllUploadTasksByState(TaskState taskState) {
        for (TransferTask tt : uploadTaskManager.getTasksByState(taskState)) {
            retryUploadTask(tt.getTaskID());
        }
    }

    public void restartUploadTasksByIds(List<Integer> ids) {
        for (int id : ids) {
            retryUploadTask(id);
        }
    }

    public void cancelUploadTaskInQue(int taskID) {
        uploadTaskManager.cancel(taskID);
        uploadTaskManager.doNext();
    }

    public void cancelAllUploadTasks() {
        uploadTaskManager.cancelAll();
        uploadTaskManager.cancelAllUploadNotification();
    }

    public void cancelUploadTasksByIds(List<Integer> ids) {
        uploadTaskManager.cancelByIds(ids);
        uploadTaskManager.cancelAllUploadNotification();
    }

    public void retryUploadTask(int taskID) {
        uploadTaskManager.retry(taskID);
    }

    public void removeUploadTask(int taskID) {
        uploadTaskManager.removeInAllTaskList(taskID);
    }

    /**
     * remove all upload tasks by their taskIds.
     *
     * Note: when deleting all tasks whose state is {@link com.seafile.seadroid2.transfer.TaskState#TRANSFERRING} in the queue,
     * other tasks left will never be executed, because they are all in the {@link com.seafile.seadroid2.transfer.TaskState#INIT} state.
     * In this case, explicitly call doNext to start processing the queue.
     *
     * @param ids
     */
    public void removeUploadTasksByIds(List<Integer> ids) {
        uploadTaskManager.removeByIds(ids);
        // explicitly call doNext if there aren`t any tasks under transferring state,
        // in case that all tasks are waiting in the queue.
        // This could happen if all transferring tasks are removed by calling removeByIds.
        if (!uploadTaskManager.isTransferring())
            uploadTaskManager.doNext();
    }

    // -------------------------- download task --------------------//
    public int addDownloadTask(Account account, String repoName, String repoID, String path) {
        return addDownloadTask(account, repoName, repoID, path, -1L);
    }

    public int addDownloadTask(Account account, String repoName, String repoID, String path, long fileSize) {
        return downloadTaskManager.addTask(account, repoName, repoID, path, fileSize);
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

    public void restartAllDownloadTasksByState(TaskState taskState) {
        for (TransferTask tt : downloadTaskManager.getTasksByState(taskState)) {
            retryDownloadTask(tt.getTaskID());
        }
    }

    public void restartDownloadTasksByIds(List<Integer> ids) {
        for (int id : ids) {
            retryDownloadTask(id);
        }
    }

    public void removeAllDownloadTasksByState(TaskState taskState) {
        downloadTaskManager.removeByState(taskState);
    }

    /**
     * remove all download tasks by their taskIds.
     *
     * Note: when deleting all tasks whose state is {@link com.seafile.seadroid2.transfer.TaskState#TRANSFERRING} in the queue,
     * other tasks left will never be executed, because they are all in the {@link com.seafile.seadroid2.transfer.TaskState#INIT} state.
     * In this case, explicitly call doNext to start processing the queue.
     *
     * @param ids
     */
    public void removeDownloadTasksByIds(List<Integer> ids) {
        downloadTaskManager.removeByIds(ids);
        // explicitly call doNext if there aren`t any tasks under transferring state,
        // in case that all tasks are waiting in the queue.
        // This could happen if all transferring tasks are removed by calling removeByIds.
        if (!downloadTaskManager.isTransferring())
            downloadTaskManager.doNext();
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

    public void cancellDownloadTasksByIds(List<Integer> ids) {
        downloadTaskManager.cancelByIds(ids);
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
