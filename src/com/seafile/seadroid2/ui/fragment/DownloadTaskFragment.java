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
 * Download tasks fragments
 *
 * Created by Logan on 14/12/22.
 */
public class DownloadTaskFragment extends TransferTaskFragment {
    private static final String DEBUG_TAG = "DownloadTaskFragment";

    private boolean isDownloadListVisible;

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onActivityCreated");
        super.onActivityCreated(savedInstanceState);

        emptyView.setText(getString(R.string.no_download_tasks));
    }

    @Override
    void setUpTransferList(TransferService txService) {

        Log.d(DEBUG_TAG, "bind TransferService");
        List<DownloadTaskInfo> infos = txService.getAllDownloadTaskInfos();
        adapter = new TransferTaskAdapter(mActivity, null, infos);
        adapter.setCurrentTab(TransferTaskAdapter.DOWNLOAD_LIST_TAB);
        mTransferTaskListView.setAdapter(adapter);

    }

    @Override
    public void onResume() {
        Log.d(DEBUG_TAG, "onResume");
        isDownloadListVisible = true;
        super.onResume();
    }

    @Override
    public void onStop() {
        Log.d(DEBUG_TAG, "onStop");
        super.onStop();
        isDownloadListVisible = false;
    }

    boolean isNeedUpdateProgress() {
        // first download list should at foreground
        if (!isDownloadListVisible) {
            return false;
        }

        // second there are some downloading tasks
        if (txService == null)
            return false;

        if (!txService.isDownloading())
            return false;

        return true;
    }

    // refresh download list by mTimer
    void startTimer() {
        Log.d(DEBUG_TAG, "timer started");
        mTimer.postDelayed(new Runnable() {

            @Override
            public void run() {
                adapter.setDownloadTaskInfos(txService.getAllDownloadTaskInfos());
                adapter.notifyDataSetChanged();
                Log.d(DEBUG_TAG, "timer post refresh signal " + System.currentTimeMillis());
                mTimer.postDelayed(this, 1 * 1000);
            }
        }, 1 * 1000);
    }

    @Override
    void refreshView() {
        List<DownloadTaskInfo> infos = txService.getAllDownloadTaskInfos();
        if (infos == null || infos.isEmpty()) {
            mTransferTaskListView.setVisibility(View.GONE);
            emptyView.setVisibility(View.VISIBLE);
        } else {
            if (isNeedUpdateProgress())
                startTimer();

            mTransferTaskListView.setVisibility(View.VISIBLE);
            emptyView.setVisibility(View.GONE);
            adapter.setDownloadTaskInfos(infos);
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
            DownloadTaskInfo taskInfo = (DownloadTaskInfo) listView.getItemAtPosition(info.position);
            TransferManager.TaskState state = taskInfo.state;
            int taskID = taskInfo.taskID;

            boolean needRefresh = false;

            switch (item.getItemId()) {
                case R.id.cancel:
                    if (state == TransferManager.TaskState.INIT || state == TransferManager.TaskState.TRANSFERRING) {
                        // txService.cancelDownloadTask(taskID);
                        txService.cancelDownloadTaskInQue(taskID);
                        needRefresh = true;
                    }
                    break;
                case R.id.retry:
                    if (state == TransferManager.TaskState.FAILED || state == TransferManager.TaskState.CANCELLED) {
                        txService.retryDownloadTask(taskID);
                        needRefresh = true;
                    }
                    break;
                case R.id.remove:
                    if (state == TransferManager.TaskState.FINISHED || state == TransferManager.TaskState.FAILED || state == TransferManager.TaskState.CANCELLED) {
                        txService.removeDownloadTask(taskID);
                        needRefresh = true;
                    }
                    break;
                case R.id.remove_all_cancelled:
                    if (state == TransferManager.TaskState.CANCELLED) {
                        txService.removeAllDownloadTasksByState(TransferManager.TaskState.CANCELLED);
                        needRefresh = true;
                    }
                    break;
                case R.id.remove_all_finished:
                    if (state == TransferManager.TaskState.FINISHED) {
                        txService.removeAllDownloadTasksByState(TransferManager.TaskState.FINISHED);
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
     * cancel all download tasks
     */
    public void cancelDownloadTasks() {
        if (txService != null) {
            txService.cancellAllDownloadTasks();
        }

        refreshView();

        // stop timer
        stopTimer();
    }

}
