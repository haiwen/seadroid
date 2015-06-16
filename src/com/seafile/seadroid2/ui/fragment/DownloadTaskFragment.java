package com.seafile.seadroid2.ui.fragment;

import android.os.Bundle;
import android.util.Log;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferTaskInfo;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;

import java.util.List;

/**
 * Download tasks fragments
 */
public class DownloadTaskFragment extends TransferTaskFragment {
    private static final String DEBUG_TAG = "DownloadTaskFragment";

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onActivityCreated");
        super.onActivityCreated(savedInstanceState);

        emptyView.setText(getString(R.string.no_download_tasks));
    }

    @Override
    protected List<? extends TransferTaskInfo> getTransferTaskInfos() {
        return txService.getAllDownloadTaskInfos();
    }

    @Override
    protected void setUpTransferList() {

        Log.d(DEBUG_TAG, "bind TransferService");
        List<DownloadTaskInfo> infos = txService.getAllDownloadTaskInfos();
        adapter = new TransferTaskAdapter(mActivity, infos);
        adapter.setCurrentTab(TransferTaskAdapter.TaskType.DOWNLOAD_TASK);
        mTransferTaskListView.setAdapter(adapter);

    }

    protected boolean isNeedUpdateProgress() {
        return !txService.getAllDownloadTaskInfos().isEmpty();
    }

    /*@Override
    public boolean onContextItemSelected(android.view.MenuItem item) {
        if (getUserVisibleHint()) {
            AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();

            if (txService == null) {
                return false;
            }

            ListView listView = mTransferTaskListView;
            DownloadTaskInfo taskInfo = (DownloadTaskInfo) listView.getItemAtPosition(info.position);
            TaskState state = taskInfo.state;
            int taskID = taskInfo.taskID;

            switch (item.getItemId()) {
                case R.id.cancel:
                    if (state == TaskState.INIT || state == TaskState.TRANSFERRING) {
                        txService.cancelDownloadTaskInQue(taskID);
                    }
                    break;
                case R.id.retry:
                    if (state == TaskState.FAILED || state == TaskState.CANCELLED) {
                        txService.retryDownloadTask(taskID);
                    }
                    break;
                case R.id.remove:
                    if (state == TaskState.FINISHED || state == TaskState.FAILED || state == TaskState.CANCELLED) {
                        txService.removeDownloadTask(taskID);
                    }
                    break;
                case R.id.remove_all_cancelled:
                    if (state == TaskState.CANCELLED) {
                        txService.removeAllDownloadTasksByState(TaskState.CANCELLED);
                    }
                    break;
                case R.id.remove_all_finished:
                    if (state == TaskState.FINISHED) {
                        txService.removeAllDownloadTasksByState(TaskState.FINISHED);
                    }
                    break;
                default:
                    return super.onContextItemSelected(item);
            }

            return true;
        } else
            return false;
    }*/

    /**
     * retry all failed tasks
     */
    public void retryAllFailedTasks() {
        if (txService != null) {
            txService.restartAllDownloadTasksByState(TaskState.FAILED);
        }
    }

    /**
     * restart all cancelled tasks
     */
    public void restartAllCancelledTasks() {
        if (txService != null) {
            txService.restartAllDownloadTasksByState(TaskState.CANCELLED);
        }
    }

    /**
     * remove all failed download tasks
     */
    public void removeAllFailedDownloadTasks() {
        if (txService != null) {
            txService.removeAllDownloadTasksByState(TaskState.FAILED);
        }
    }

    /**
     * remove all cancelled download tasks
     */
    public void removeAllCancelledDownloadTasks() {
        if (txService != null) {
            txService.removeAllDownloadTasksByState(TaskState.CANCELLED);
        }
    }

    /**
     * remove cancelled download tasks by Ids
     */
    public void removeDownloadTasksByIds(List<Integer> ids) {
        if (txService != null) {
            txService.removeDownloadTasksByIds(ids);
        }
    }

    /**
     * remove all finished download tasks
     */
    public void removeAllFinishedDownloadTasks() {
        if (txService != null) {
            txService.removeAllDownloadTasksByState(TaskState.FINISHED);
        }
    }

    /**
     * cancel all download tasks
     */
    public void cancelAllDownloadTasks() {
        if (txService != null) {
            txService.cancellAllDownloadTasks();
        }
    }

    /**
     * cancel download tasks by ids
     */
    public void cancelDownloadTasksByIds(List<Integer> ids) {
        if (txService != null) {
            txService.cancellDownloadTasksByIds(ids);
        }
    }

    @Override
    protected void deleteSelectedItems(List<Integer> ids) {
        cancelDownloadTasksByIds(ids);
        removeDownloadTasksByIds(ids);
    }

}
