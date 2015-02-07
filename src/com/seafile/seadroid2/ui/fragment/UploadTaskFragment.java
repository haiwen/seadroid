package com.seafile.seadroid2.ui.fragment;

import android.os.Bundle;
import android.widget.AdapterView;
import android.widget.ListView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferTaskInfo;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;

import java.util.List;

/**
 * Upload tasks fragments
 *
 * Created by Logan on 14/12/22.
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
        adapter.setCurrentTab(TransferTaskAdapter.UPLOAD_LIST_TAB);
        mTransferTaskListView.setAdapter(adapter);
    }

    @Override
    protected boolean isNeedUpdateProgress() {
        return !txService.getAllUploadTaskInfos().isEmpty();
    }

    @Override
    public boolean onContextItemSelected(android.view.MenuItem item) {
        if (getUserVisibleHint()) {
            AdapterView.AdapterContextMenuInfo info = (AdapterView.AdapterContextMenuInfo) item.getMenuInfo();

            if (txService == null) {
                return false;
            }

            ListView listView = mTransferTaskListView;
            UploadTaskInfo taskInfo = (UploadTaskInfo) listView.getItemAtPosition(info.position);
            TaskState state = taskInfo.state;
            int taskID = taskInfo.taskID;

            switch (item.getItemId()) {
                case R.id.cancel:
                    if (state == TaskState.INIT || state == TaskState.TRANSFERRING) {
                        txService.cancelUploadTaskInQue(taskID);
                    }
                    break;
                case R.id.retry:
                    if (state == TaskState.FAILED || state == TaskState.CANCELLED) {
                        txService.retryUploadTask(taskID);
                    }
                    break;
                case R.id.remove:
                    if (state == TaskState.FINISHED || state == TaskState.FAILED || state == TaskState.CANCELLED) {
                        txService.removeUploadTask(taskID);
                    }
                    break;
                case R.id.remove_all_cancelled:
                    if (state == TaskState.CANCELLED) {
                        txService.removeAllUploadTasksByState(TaskState.CANCELLED);
                    }
                    break;
                case R.id.remove_all_finished:
                    if (state == TaskState.FINISHED) {
                        txService.removeAllUploadTasksByState(TaskState.FINISHED);
                    }
                    break;
                default:
                    return super.onContextItemSelected(item);
            }

            return true;
        }
        else
            return false;
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
}