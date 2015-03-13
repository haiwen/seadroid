package com.seafile.seadroid2.transfer;

import android.content.Intent;
import android.support.v4.content.LocalBroadcastManager;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Download task manager
 * <p/>
 */
public class DownloadTaskManager extends TransferManager implements DownloadStateListener {
    private static final String DEBUG_TAG = "DownloadTaskManager";

    public static final String BROADCAST_FILE_DOWNLOAD_SUCCESS = "downloaded";
    public static final String BROADCAST_FILE_DOWNLOAD_FAILED = "downloadFailed";
    public static final String BROADCAST_FILE_DOWNLOAD_PROGRESS = "downloadProgress";

    /**
     * Add a new download task.
     * call this method to execute a task immediately.
     */
    public int addTask(Account account, String repoName, String repoID, String path) {
        TransferTask task = new DownloadTask(++notificationID, account,
                repoName, repoID, path, this);
        TransferTask oldTask = null;
        if (allTaskList.contains(task)) {
            oldTask = allTaskList.get(allTaskList.indexOf(task));
        }
        if (oldTask != null) {
            if (oldTask.getState().equals(TaskState.CANCELLED)
                    || oldTask.getState().equals(TaskState.FAILED)
                    || oldTask.getState().equals(TaskState.FINISHED)) {
                allTaskList.remove(oldTask);
            } else {
                // return taskID of old task
                return oldTask.getTaskID();
            }
        }
        allTaskList.add(task);
        ConcurrentAsyncTask.execute(task);
        return task.getTaskID();
    }

    public void addTaskToQue(Account account, String repoName, String repoID, String path) {
        // create a new one to avoid IllegalStateException
        DownloadTask downloadTask = new DownloadTask(++notificationID, account, repoName, repoID, path, this);
        addTaskToQue(downloadTask);
    }

    public int getDownloadingFileCountByPath(String repoID, String dir) {
        List<DownloadTaskInfo> downloadTaskInfos = getTaskInfoListByPath(repoID, dir);
        int count = 0;
        for (DownloadTaskInfo downloadTaskInfo : downloadTaskInfos) {
            if (downloadTaskInfo.state.equals(TaskState.INIT)
                    || downloadTaskInfo.state.equals(TaskState.TRANSFERRING))
                count++;
        }
        return count;
    }

    /**
     * get all download task info under a specific directory.
     *
     * @param repoID
     * @param dir
     * @return List<DownloadTaskInfo>
     */
    public List<DownloadTaskInfo> getTaskInfoListByPath(String repoID, String dir) {
        ArrayList<DownloadTaskInfo> infos = Lists.newArrayList();
        for (TransferTask task : allTaskList) {
            if (!task.getRepoID().equals(repoID))
                continue;

            String parentDir = Utils.getParentPath(task.getPath());

            if (parentDir.equals(dir))
                infos.add(((DownloadTask) task).getTaskInfo());
        }

        return infos;
    }

    public void retry(int taskID) {
        DownloadTask task = (DownloadTask) getTask(taskID);
        if (task == null || !task.canRetry())
            return;
        addTaskToQue(task.getAccount(), task.getRepoName(), task.getRepoID(), task.getPath());
    }

    // -------------------------- listener method --------------------//
    @Override
    public void onFileDownloadProgress(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_DOWNLOAD_PROGRESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(SeadroidApplication.getAppContext()).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloaded(int taskID) {
        remove(taskID);
        doNext();
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_DOWNLOAD_SUCCESS).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(SeadroidApplication.getAppContext()).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloadFailed(int taskID) {
        remove(taskID);
        doNext();
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type",
                BROADCAST_FILE_DOWNLOAD_FAILED).putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(SeadroidApplication.getAppContext()).sendBroadcast(localIntent);
    }
}
