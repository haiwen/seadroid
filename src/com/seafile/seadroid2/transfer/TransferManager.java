package com.seafile.seadroid2.transfer;

import android.util.Log;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Manages file downloading and uploading.
 * <p/>
 * Currently use an AsyncTask for an file.
 */
public abstract class TransferManager {
    private static final String DEBUG_TAG = "TransferManager";

    public static final String BROADCAST_ACTION = "com.seafile.seadroid.TX_BROADCAST";

    /**
     * unique task id
     */
    protected int notificationID;

    protected static final int TRANSFER_MAX_COUNT = 2;
    /**
     * contains all transfer tasks, including failed, cancelled, finished, transferring, waiting tasks.
     */
    protected List<TransferTask> allTaskList = new CopyOnWriteArrayList();
    /**
     * contains currently transferring tasks
     */
    protected List<TransferTask> transferringList = new CopyOnWriteArrayList();
    /**
     * contains waiting tasks
     */
    protected List<TransferTask> waitingList = new CopyOnWriteArrayList();

    protected TransferTask getTask(int taskID) {
        for (TransferTask task : allTaskList) {
            if (task.getTaskID() == taskID) {
                return task;
            }
        }
        return null;
    }

    public TransferTaskInfo getTaskInfo(int taskID) {
        TransferTask task = getTask(taskID);
        if (task != null) {
            return task.getTaskInfo();
        }

        return null;
    }

    private boolean hasInQue(TransferTask transferTask) {
        if (waitingList.contains(transferTask)) {
            // Log.d(DEBUG_TAG, "in  Que  " + taskID + " " + repoName + path + "in waiting list");
            return true;
        }

        if (transferringList.contains(transferTask)) {
            // Log.d(DEBUG_TAG, "in  Que  " + taskID + " " + repoName + path + " in downloading list");
            return true;
        }
        return false;
    }

    protected void addTaskToQue(TransferTask task) {
        if (!hasInQue(task)) {
            // remove the cancelled or failed task if any
            allTaskList.remove(task);

            // add new created task
            allTaskList.add(task);

            // Log.d(DEBUG_TAG, "add Que  " + taskID + " " + repoName + path);
            waitingList.add(task);
            doNext();
        }
    }

    public void doNext() {
        if (!waitingList.isEmpty()
                && transferringList.size() < TRANSFER_MAX_COUNT) {
            Log.d(DEBUG_TAG, "do next!");

            TransferTask task = waitingList.remove(0);
            transferringList.add(task);

            ConcurrentAsyncTask.execute(task);
        }
    }

    protected void cancel(int taskID) {
        TransferTask task = getTask(taskID);
        if (task != null) {
            task.cancel();

        }

        remove(taskID);
    }

    protected void remove(int taskID) {

        TransferTask toCancel = getTask(taskID);
        if (toCancel == null)
            return;

        if (!waitingList.isEmpty()) {
            waitingList.remove(toCancel);
        }

        if (!transferringList.isEmpty()) {
            transferringList.remove(toCancel);
        }
    }

    public void removeInAllTaskList(int taskID) {
        TransferTask task = getTask(taskID);
        if (task != null) {
            allTaskList.remove(task);
        }
    }

    public void removeByState(TaskState taskState) {
        Iterator<TransferTask> iter = allTaskList.iterator();
        while (iter.hasNext()) {
            TransferTask task = iter.next();
            if (task.getState().equals(taskState)) {
                iter.remove();
            }
        }
    }

    public void cancelAll() {
        List<? extends TransferTaskInfo> transferTaskInfos = getAllTaskInfoList();
        for (TransferTaskInfo transferTaskInfo : transferTaskInfos) {
            cancel(transferTaskInfo.taskID);
        }
    }

    public List<? extends TransferTaskInfo> getAllTaskInfoList() {
        ArrayList<TransferTaskInfo> infos = Lists.newArrayList();
        for (TransferTask task : allTaskList) {
            infos.add(task.getTaskInfo());
        }

        return infos;
    }
}
