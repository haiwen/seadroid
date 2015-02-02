package com.seafile.seadroid2.ui.fragment;

import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.*;
import com.seafile.seadroid2.ui.adapter.TransferTaskAdapter;

import java.util.List;

/**
 * Upload tasks fragments
 *
 * Created by Logan on 14/12/22.
 */
public class UploadTaskFragment extends TransferTaskFragment {
    private static final String DEBUG_TAG = "UploadTaskFragment";

    private boolean isUploadListVisible;

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

        emptyView.setText(getString(R.string.no_upload_tasks));

    }

    @Override
    void setUpTransferList(TransferService txService) {
        List<UploadTaskInfo> infos = txService.getAllUploadTaskInfos();
        adapter = new TransferTaskAdapter(mActivity, infos, null);
        adapter.setCurrentTab(TransferTaskAdapter.UPLOAD_LIST_TAB);
        mTransferTaskListView.setAdapter(adapter);
    }

    @Override
    public void onResume() {
        isUploadListVisible = true;
        super.onResume();
    }

    boolean isNeedUpdateProgress() {
        // first upload list should at foreground
        if (!isUploadListVisible) {
            return false;
        }

        // second there are some upload tasks
        if(txService == null)
            return false;

        if (!txService.isUploading())
            return false;

        return true;
    }

    @Override
    public void onStop() {
        super.onStop();
        isUploadListVisible = false;
    }

    // refresh upload list by mTimer
    void startTimer() {
        Log.d(DEBUG_TAG, "timer started");
        mTimer.postDelayed(new Runnable() {

            @Override
            public void run() {
                adapter.setUploadTaskInfos(txService.getAllUploadTaskInfos());
                adapter.notifyDataSetChanged();
                Log.d(DEBUG_TAG, "timer post refresh signal " + System.currentTimeMillis());
                mTimer.postDelayed(this, 1 * 1000);
            }
        }, 1 * 1000);
    }

    void refreshView() {

        List<UploadTaskInfo> infos = txService.getAllUploadTaskInfos();
        if (infos == null || infos.isEmpty()) {
            mTransferTaskListView.setVisibility(View.GONE);
            emptyView.setVisibility(View.VISIBLE);
        } else {
            if (isNeedUpdateProgress())
                startTimer();

            mTransferTaskListView.setVisibility(View.VISIBLE);
            emptyView.setVisibility(View.GONE);
            adapter.setUploadTaskInfos(infos);
            adapter.notifyDataSetChanged();
        }

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
            TransferManager.TaskState state = taskInfo.state;
            int taskID = taskInfo.taskID;

            boolean needRefresh = false;

            switch (item.getItemId()) {
                case R.id.cancel:
                    if (state == TransferManager.TaskState.INIT || state == TransferManager.TaskState.TRANSFERRING) {
                        // txService.cancelUploadTask(taskID);
                        txService.cancelUploadTaskInQue(taskID);
                        needRefresh = true;
                    }
                    break;
                case R.id.retry:
                    if (state == TransferManager.TaskState.FAILED || state == TransferManager.TaskState.CANCELLED) {
                        txService.retryUploadTask(taskID);
                        needRefresh = true;
                    }
                    break;
                case R.id.remove:
                    if (state == TransferManager.TaskState.FINISHED || state == TransferManager.TaskState.FAILED || state == TransferManager.TaskState.CANCELLED) {
                        txService.removeUploadTask(taskID);
                        needRefresh = true;
                    }
                    break;
                case R.id.remove_all_cancelled:
                    if (state == TransferManager.TaskState.CANCELLED) {
                        txService.removeAllUploadTasksByState(TransferManager.TaskState.CANCELLED);
                        needRefresh = true;
                    }
                    break;
                case R.id.remove_all_finished:
                    if (state == TransferManager.TaskState.FINISHED) {
                        txService.removeFinishedUploadTasks();
                        needRefresh = true;
                    }
                    break;
                default:
                    return super.onContextItemSelected(item);
            }

            if (needRefresh) {
                refreshView();
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

        refreshView();

        // stop timer
        stopTimer();
    }
}