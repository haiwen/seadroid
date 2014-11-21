package com.seafile.seadroid2.ui.activity;

import java.util.List;

import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.View;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.ListView;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.TransferManager.TaskState;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.adapter.UploadTasksAdapter;

public class UploadTasksActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "UploadTasksActivity";

    private ListView uploadTasksView;
    private UploadTasksAdapter adapter;
    private TransferService txService;
    private TransferReceiver mTransferReceiver;
    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferBinder binder = (TransferBinder) service;
            txService = binder.getService();
            Log.d(DEBUG_TAG, "bind TransferService");
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };
    
    protected void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "UploadTasksActivity.onCreate is called");
        super.onCreate(savedInstanceState);
        
        setContentView(R.layout.upload_tasks_activity);
        this.supportInvalidateOptionsMenu();
        adapter = BrowserActivity.uploadTasksAdapter;
        uploadTasksView = (ListView) findViewById(R.id.upload_tasks_list);
        uploadTasksView.setAdapter(adapter);
        if (adapter != null && adapter.isEmpty()) {
            uploadTasksView.setVisibility(View.GONE);
        }
        if (adapter == null) {
            finish();
        }
        registerForContextMenu(uploadTasksView);
        
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
        
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);     
    }

    @Override
    protected void onResume() {
        super.onResume();
        Log.d(DEBUG_TAG, "datasetChanged");
        adapter.notifyDataSetChanged();
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

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }

        super.onDestroy();
    }

    private List<UploadTaskInfo> getUploadTaskInfos() {
        if (txService == null) {
            // In case the service is not ready
            return Lists.newArrayList();
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
