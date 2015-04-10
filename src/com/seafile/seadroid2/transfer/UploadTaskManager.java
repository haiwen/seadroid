package com.seafile.seadroid2.transfer;

import android.content.Intent;
import android.support.v4.content.LocalBroadcastManager;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Upload task manager
 * <p/>
 */
public class UploadTaskManager extends TransferManager implements UploadStateListener {
    private static final String DEBUG_TAG = "UploadTaskManager";

    public static final String BROADCAST_FILE_UPLOAD_SUCCESS = "uploaded";
    public static final String BROADCAST_FILE_UPLOAD_FAILED = "uploadFailed";
    public static final String BROADCAST_FILE_UPLOAD_PROGRESS = "uploadProgress";
    public static final String BROADCAST_FILE_UPLOAD_CANCELLED = "uploadCancelled";

    public void addTaskToQue(Account account, String repoID, String repoName, String dir, String filePath, boolean isUpdate, boolean isCopyToLocal) {
        if (repoID == null || repoName == null)
            return;

        // create a new one to avoid IllegalStateException
        UploadTask task = new UploadTask(++notificationID, account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal, this);
        addTaskToQue(task);
    }

    public void cancelAllCameraUploadTasks() {
        List<UploadTaskInfo> uploadTaskInfos = (List<UploadTaskInfo>) getAllTaskInfoList();
        for (UploadTaskInfo uploadTaskInfo : uploadTaskInfos) {
            // use isCopyToLocal as a flag to mark a camera photo upload task if false
            // mark a file upload task if true
            if (!uploadTaskInfo.isCopyToLocal) {
                cancel(uploadTaskInfo.taskID);
            }
        }
    }

    /**
     * get all upload task info under a specific directory.
     *
     * @param repoID
     * @param dir
     * @return List<UploadTaskInfo>
     */
    private List<UploadTaskInfo> getTaskInfoListByPath(String repoID, String dir) {
        ArrayList<UploadTaskInfo> infos = Lists.newArrayList();
        for (TransferTask task : allTaskList) {
            if (!task.getRepoID().equals(repoID))
                continue;

            UploadTaskInfo uti = (UploadTaskInfo) getTaskInfo(task.getTaskID());
            if (dir.equals(uti.parentDir))
                infos.add(((UploadTask) task).getTaskInfo());
        }

        return infos;
    }

    public long getUploadedFileSizeByPath(String repoID, String dir) {
        long uploadedSize = 0l;
        List<UploadTaskInfo> list = getTaskInfoListByPath(repoID, dir);
        for (UploadTaskInfo uti : list) {
            if (uti.state.equals(TaskState.FINISHED))
                uploadedSize += uti.totalSize;
            else if (uti.state.equals(TaskState.TRANSFERRING))
                uploadedSize += uti.uploadedSize;
        }
        return uploadedSize;
    }

    public int getUploadingFileCountByPath(String repoID, String dir) {
        List<UploadTaskInfo> taskInfos = getTaskInfoListByPath(repoID, dir);
        int count = 0;
        for (UploadTaskInfo uti : taskInfos) {
            if (uti.state.equals(TaskState.INIT)
                    || uti.state.equals(TaskState.TRANSFERRING))
                count++;
        }
        return count;
    }

    public int getUploadingFileCount() {
        List<UploadTaskInfo> taskInfos = (List<UploadTaskInfo>) getAllTaskInfoList();
        int count = 0;
        for (UploadTaskInfo uti : taskInfos) {
            if (uti.state.equals(TaskState.INIT)
                    || uti.state.equals(TaskState.TRANSFERRING))
                count++;
        }
        return count;
    }

    public void retry(int taskID) {
        UploadTask task = (UploadTask) getTask(taskID);
        if (task == null || !task.canRetry())
            return;
        addTaskToQue(task.getAccount(), task.getRepoID(), task.getRepoName(), task.getDir(), task.getPath(), task.isUpdate(), task.isCopyToLocal());
    }

    // -------------------------- listener method --------------------//
    @Override
    public void onFileUploadProgress(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_PROGRESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(SeadroidApplication.getAppContext()).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploaded(int taskID) {
        remove(taskID);
        doNext();
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_SUCCESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(SeadroidApplication.getAppContext()).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadCancelled(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_CANCELLED).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(SeadroidApplication.getAppContext()).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadFailed(int taskID) {
        remove(taskID);
        doNext();
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_UPLOAD_FAILED).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(SeadroidApplication.getAppContext()).sendBroadcast(localIntent);
    }

}
