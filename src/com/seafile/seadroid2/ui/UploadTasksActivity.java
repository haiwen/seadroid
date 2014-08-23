package com.seafile.seadroid2.ui;

import java.util.ArrayList;
import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.View;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.ListView;
import android.support.v4.content.LocalBroadcastManager;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferManager.TaskState;
import com.seafile.seadroid2.transfer.UploadTaskInfo;

public class UploadTasksActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "UploadTasksActivity";

    private ListView uploadTasksView;
    private UploadTasksAdapter adapter;
    private TransferService txService;
    TransferReceiver mTransferReceiver;
    
    protected void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "UploadTasksActivity.onCreate is called");
        super.onCreate(savedInstanceState);
        txService = BrowserActivity.getTransferService();
        
        setContentView(R.layout.upload_tasks_activity);
        this.supportInvalidateOptionsMenu();
        adapter = new UploadTasksAdapter(this, getUploadTaskInfos());
        uploadTasksView = (ListView) findViewById(R.id.upload_tasks_list);
        uploadTasksView.setAdapter(adapter);
        registerForContextMenu(uploadTasksView);
        
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);     
    }

    @Override
    public void onStart() {
        Log.d(DEBUG_TAG, "onStart");
        super.onStart();

        if (mTransferReceiver == null) {
            mTransferReceiver = new TransferReceiver();
        }

        IntentFilter filter = new IntentFilter(TransferService.BROADCAST_ACTION);
        LocalBroadcastManager.getInstance(this).registerReceiver(mTransferReceiver, filter);

    }

    @Override
    protected void onStop() {
        Log.d(DEBUG_TAG, "onStop");
        super.onStop();

        if (mTransferReceiver != null) {
            LocalBroadcastManager.getInstance(this).unregisterReceiver(mTransferReceiver);
        }
    }

    private List<UploadTaskInfo> getUploadTaskInfos() {
        if (txService == null) {
            // In case the service is not ready
            return new ArrayList<UploadTaskInfo>();
        }

        return txService.getAllUploadTaskInfos();
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v,
            ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
        android.view.MenuInflater inflater = this.getMenuInflater();
        inflater.inflate(R.menu.upload_task_menu, menu);

        ListView listView = (ListView)v;
        AdapterContextMenuInfo info = (AdapterContextMenuInfo) menuInfo;
        UploadTaskInfo taskInfo = (UploadTaskInfo)listView.getItemAtPosition(info.position);

        android.view.MenuItem itemCancel = menu.findItem(R.id.cancel);
        android.view.MenuItem itemRetry = menu.findItem(R.id.retry);
        android.view.MenuItem itemRemove = menu.findItem(R.id.remove);
        android.view.MenuItem itemRemoveAll = menu.findItem(R.id.remove_all_finished);

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
    public boolean onContextItemSelected(android.view.MenuItem item) {
        AdapterContextMenuInfo info = (AdapterContextMenuInfo) item.getMenuInfo();

        if (txService == null) {
            return false;
        }

        ListView listView = uploadTasksView;
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

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
         switch (item.getItemId()) {
            case android.R.id.home:
                this.finish();
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    public boolean isReady() {
        return adapter != null;
    }

    public void refreshView() {
        adapter.setTaskInfos(getUploadTaskInfos());
        adapter.notifyDataSetChanged();
        this.supportInvalidateOptionsMenu();
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

    private void onFileUploadProgress(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (isReady())
            onTaskProgressUpdate(info);
    }

    private void onFileUploaded(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (isReady())
            onTaskFinished(info);
    }

    private void onFileUploadCancelled(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (isReady())
            onTaskCancelled(info);
    }

    private void onFileUploadFailed(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (isReady())
            onTaskFailed(info);
    }

    // for receive broadcast from TransferService
    private class TransferReceiver extends BroadcastReceiver {

        private TransferReceiver() {}

        public void onReceive(Context context, Intent intent) {
            String type = intent.getStringExtra("type");
            if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploaded(taskID);

            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadFailed(taskID);

            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_PROGRESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadProgress(taskID);
            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_CANCELLED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadCancelled(taskID);
            }
        }

    } // TransferReceiver
}
