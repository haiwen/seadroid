package com.seafile.seadroid2.ui.fragment;

import android.os.Bundle;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferTaskInfo;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;

import java.util.List;

/**
 * Upload tasks fragments
 *
 */
public class UploadTaskFragment extends TransferTaskFragment {
    private static final String DEBUG_TAG = "UploadTaskFragment";

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

        emptyView.setText(getString(R.string.no_upload_tasks));

    }

    @Override
    protected List<? extends TransferTaskInfo> getTransferTaskInfos() {
        return txService.getAllUploadTaskInfos();
    }

    @Override
    protected void setUpTransferList() {
        List<UploadTaskInfo> infos = txService.getAllUploadTaskInfos();
        adapter = new TransferTaskAdapter(mActivity, infos);
        adapter.setCurrentTab(TransferTaskAdapter.TaskType.UPLOAD_TASK);
        mTransferTaskListView.setAdapter(adapter);
    }

    @Override
    protected boolean isNeedUpdateProgress() {
        return !txService.getAllUploadTaskInfos().isEmpty();
    }


    /**
     * retry all failed tasks
     */
    public void retryAllFailedTasks() {
        if (txService != null) {
            txService.restartAllUploadTasksByState(TaskState.FAILED);
        }
    }

    /**
     * restart all cancelled tasks
     */
    public void restartAllCancelledTasks() {
        if (txService != null) {
            txService.restartAllUploadTasksByState(TaskState.CANCELLED);
        }
    }

    public void restartTasksByIds(List<Integer> ids) {
        if (txService != null) {
            txService.restartUploadTasksByIds(ids);
        }
    }

    /**
     * remove all failed Upload tasks
     */
    public void removeAllFailedUploadTasks() {
        if (txService != null) {
            txService.removeAllUploadTasksByState(TaskState.FAILED);
        }
    }

    /**
     * remove all {@link TaskState#FINISHED}, {@link TaskState#FAILED} and {@link TaskState#CANCELLED} Upload tasks
     */
    public void removeAllUploadTasks() {
        if (txService != null) {
            txService.removeAllUploadTasksByState(TaskState.FINISHED);
            txService.removeAllUploadTasksByState(TaskState.FAILED);
            txService.removeAllUploadTasksByState(TaskState.CANCELLED);
        }
    }

    /**
     * remove all finished Upload tasks
     */
    public void removeAllFinishedUploadTasks() {
        if (txService != null) {
            txService.removeAllUploadTasksByState(TaskState.FINISHED);
        }
    }

    /**
     *  cancel upload tasks
     *  Note that, this method <strong>only</strong> use to cancel file upload tasks rather than Camera Upload tasks.
     *  Because Camera Upload tasks will restarted again by Camera Upload Service.
     *  If want to persistently turn off Camera Upload tasks, should turn off the Camera upload service in Settings Menu.
     */
    public void cancelUploadTasks() {
        if (txService != null) {
            txService.cancelAllUploadTasks();
        }
    }

    private void cancelUploadTasksByIds(List<Integer> ids) {
        if (txService != null) {
            txService.cancelUploadTasksByIds(ids);
        }
    }

    private void removeUploadTasksByIds(List<Integer> ids) {
        if (txService != null) {
            txService.removeUploadTasksByIds(ids);
        }

    }

    @Override
    protected void deleteSelectedItems(List<Integer> ids) {
        cancelUploadTasksByIds(ids);
        removeUploadTasksByIds(ids);
    }

    @Override
    protected void restartSelectedItems(List<Integer> ids) {
        restartTasksByIds(ids);
        removeUploadTasksByIds(ids);
    }

}