package com.seafile.seadroid2.ui;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.ListView;

import com.actionbarsherlock.app.SherlockListFragment;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferManager.TaskState;
import com.seafile.seadroid2.transfer.TransferManager.UploadTaskInfo;

public class UploadTasksFragment extends SherlockListFragment {
    private static final String DEBUG_TAG = "UploadTasksFragment";

    private UploadTasksAdapter adapter;
    BrowserActivity mActivity = null;

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "UploadTasksFragment Attached");
        mActivity = (BrowserActivity)activity;
    }

    @Override
    public void onDetach() {
        mActivity = null;
        super.onDetach();
    }

    private List<UploadTaskInfo> getUploadTaskInfos() {
        TransferService txService = mActivity.getTransferService();
        if (txService == null) {
            // In case the service is not ready
            return new ArrayList<UploadTaskInfo>();
        }

        return txService.getAllUploadTaskInfos();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        return inflater.inflate(R.layout.upload_tasks_fragment, container, false);
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        adapter = new UploadTasksAdapter(getActivity(),
                                         getUploadTaskInfos());
        setListAdapter(adapter);
        registerForContextMenu(getListView());
        mActivity.invalidateOptionsMenu();
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v,
            ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
        MenuInflater inflater = mActivity.getMenuInflater();
        inflater.inflate(R.menu.upload_task_menu, menu);

        ListView listView = (ListView)v;
        AdapterContextMenuInfo info = (AdapterContextMenuInfo) menuInfo;
        UploadTaskInfo taskInfo = (UploadTaskInfo)listView.getItemAtPosition(info.position);

        MenuItem itemCancel = menu.findItem(R.id.cancel);
        MenuItem itemRetry = menu.findItem(R.id.retry);
        MenuItem itemRemove = menu.findItem(R.id.remove);
        MenuItem itemRemoveAll = menu.findItem(R.id.remove_all_finished);

        itemCancel.setVisible(false);
        itemRetry.setVisible(false);
        itemRemove.setVisible(false);
        itemRemoveAll.setVisible(false);

        switch (taskInfo.state) {
        case INIT:
            itemCancel.setVisible(true);
            break;
        case TRANSFERRING:
            itemCancel.setVisible(true);
            break;
        case CANCELLED:
            itemRetry.setVisible(true);
            itemRemove.setVisible(true);
            break;
        case FAILED:
            itemRetry.setVisible(true);
            itemRemove.setVisible(true);
            break;
        case FINISHED:
            itemRemove.setVisible(true);
            itemRemoveAll.setVisible(true);
            break;
        }
    }

    @Override
    public boolean onContextItemSelected(MenuItem item) {
        AdapterContextMenuInfo info = (AdapterContextMenuInfo) item.getMenuInfo();
        TransferService txService = mActivity.getTransferService();

        if (txService == null) {
            return false;
        }

        ListView listView = getListView();
        UploadTaskInfo taskInfo = (UploadTaskInfo)listView.getItemAtPosition(info.position);
        TaskState state = taskInfo.state;
        int taskID = taskInfo.taskID;

        boolean needRefresh = false;

        switch (item.getItemId()) {
        case R.id.cancel:
            if (state == TaskState.INIT || state == TaskState.TRANSFERRING) {
                txService.cancelUploadTask(taskID);
                needRefresh = true;
            }
            break;
        case R.id.retry:
            if (state == TaskState.FAILED || state == TaskState.CANCELLED) {
                txService.retryUploadTask(taskID);
                needRefresh = true;
            }
            break;
        case R.id.remove:
            if (state == TaskState.FINISHED || state == TaskState.FAILED || state == TaskState.CANCELLED) {
                txService.removeUploadTask(taskID);
                needRefresh = true;
            }
            break;
        case R.id.remove_all_finished:
            if (state == TaskState.FINISHED) {
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

    public boolean isReady() {
        return adapter != null;
    }

    public void refreshView() {
        adapter.setTaskInfos(getUploadTaskInfos());
        adapter.notifyDataSetChanged();
        mActivity.invalidateOptionsMenu();
    }

    public void onTaskProgressUpdate(UploadTaskInfo info) {
        adapter.onTaskProgressUpdate(info);
    }

    public void onTaskFinished(UploadTaskInfo info) {
        adapter.onTaskFinished(info);
    }

    public void onTaskFailed(UploadTaskInfo info) {
        adapter.onTaskFailed(info);
    }

    public void onTaskCancelled(UploadTaskInfo info) {
        adapter.onTaskCancelled(info);
    }
}