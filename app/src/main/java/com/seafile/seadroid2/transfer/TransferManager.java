package com.seafile.seadroid2.transfer;

import android.util.Log;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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
    protected List<TransferTask> allTaskList = Lists.newArrayList();
    /**
     * contains currently transferring tasks
     */
    protected List<TransferTask> transferringList = Lists.newArrayList();
    /**
     * contains waiting tasks
     */
    protected List<TransferTask> waitingList = Lists.newArrayList();

    protected synchronized TransferTask getTask(int taskID) {
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

    private synchronized boolean hasInQue(TransferTask transferTask) {
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
            synchronized (this) {
                allTaskList.remove(task);

                // add new created task
                allTaskList.add(task);

                // Log.d(DEBUG_TAG, "add Que  " + taskID + " " + repoName + path);
                waitingList.add(task);
            }
            doNext();
        }
    }

    public synchronized void doNext() {
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

    protected synchronized void remove(int taskID) {

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
            synchronized (this) {
                allTaskList.remove(task);
            }
        }
    }

    public synchronized List<TransferTask> getTasksByState(TaskState taskState) {
        List<TransferTask> taskList = Lists.newArrayList();
        Iterator<TransferTask> iter = allTaskList.iterator();
        while (iter.hasNext()) {
            TransferTask task = iter.next();
            if (task.state.equals(taskState)) {
                taskList.add(task);
            }
        }
        return taskList;
    }

    /**
     * remove tasks from {@link #allTaskList} by comparing the taskState,
     * all tasks with the same taskState will be removed.
     *
     * @param taskState
     *          taskState
     */
    public synchronized void removeByState(TaskState taskState) {
        Iterator<TransferTask> iter = allTaskList.iterator();
        while (iter.hasNext()) {
            TransferTask task = iter.next();
            if (task.getState().equals(taskState)) {
                iter.remove();
            }
        }
    }

    /**
     * remove tasks from {@link #allTaskList} by traversing the taskId list
     *
     * @param ids
     *          taskId list
     */
    public synchronized void removeByIds(List<Integer> ids) {
        for (int taskID : ids) {
            TransferTask transferTask = getTask(taskID);
            allTaskList.remove(transferTask);
        }
    }

    /**
     * check if there are tasks under transferring state
     *
     * @return true, if there are tasks whose {@link com.seafile.seadroid2.transfer.TaskState} is {@code TRANSFERRING}.
     *          false, otherwise.
     */
    public boolean isTransferring() {
        List<? extends TransferTaskInfo> transferTaskInfos = getAllTaskInfoList();
        for (TransferTaskInfo transferTaskInfo : transferTaskInfos) {
            if (transferTaskInfo.state.equals(TaskState.TRANSFERRING))
                return true;
        }
        return false;
    }

    public void cancelAll() {
        List<? extends TransferTaskInfo> transferTaskInfos = getAllTaskInfoList();
        for (TransferTaskInfo transferTaskInfo : transferTaskInfos) {
            cancel(transferTaskInfo.taskID);
        }
    }

    public void cancelByIds(List<Integer> taskIds) {
        for (int taskID : taskIds) {
            cancel(taskID);
        }
    }

    public synchronized List<? extends TransferTaskInfo> getAllTaskInfoList() {
        ArrayList<TransferTaskInfo> infos = Lists.newArrayList();
        for (TransferTask task : allTaskList) {
            infos.add(task.getTaskInfo());
        }

        return infos;
    }

}
