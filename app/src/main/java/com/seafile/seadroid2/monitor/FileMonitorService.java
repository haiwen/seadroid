package com.seafile.seadroid2.monitor;

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.os.Binder;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.transfer.*;

/**
 * Monitor changes of local cached files, and upload them through TransferService if modified
 */
public class FileMonitorService extends Service {
    private static final String DEBUG_TAG = "FileMonitorService";

    private SeafileMonitor monitor;
    private TransferService mTransferService;
    private AutoUpdateManager updateMgr = new AutoUpdateManager();
    private final IBinder mBinder = new MonitorBinder();

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand called.");

        if (monitor == null) {
            monitor = new SeafileMonitor(updateMgr);
        }

        ConcurrentAsyncTask.submit(new Runnable() {
            @Override
            public void run() {
                monitor.monitorAllAccounts();
            }
        });

        return START_STICKY;
    }

    public class MonitorBinder extends Binder {
        public FileMonitorService getService() {
            return FileMonitorService.this;
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        Log.d(DEBUG_TAG, "onBind");
        return mBinder;
    }

    @Override
    public void onCreate() {
        Log.d(DEBUG_TAG, "onCreate");

        Intent bindIntent = new Intent(this, TransferService.class);
        bindService(bindIntent, mTransferConnection, Context.BIND_AUTO_CREATE);

        LocalBroadcastManager.getInstance(this).registerReceiver(transferReceiver,
                new IntentFilter(TransferManager.BROADCAST_ACTION));
    }

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");

        updateMgr.stop();

        if (monitor != null) {
            try {
                monitor.stop();
            } catch (Exception e) {
                Log.d(DEBUG_TAG, "failed to stop file monitor");
            }
        }

        if (mTransferService != null) {
            unbindService(mTransferConnection);
            mTransferService = null;
        }

        LocalBroadcastManager.getInstance(this).unregisterReceiver(transferReceiver);
    }

    public void removeAccount(Account account) {
        Log.d(DEBUG_TAG, account.email);
        if (monitor != null)
            monitor.stopMonitorFilesForAccount(account);
    }

    private ServiceConnection mTransferConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName className, IBinder binder) {
            TransferService.TransferBinder transferBinder = (TransferService.TransferBinder) binder;
            mTransferService = transferBinder.getService();
            updateMgr.onTransferServiceConnected(mTransferService);
        }

        @Override
        public void onServiceDisconnected(ComponentName className) {
            mTransferService = null;
        }

    };

    private BroadcastReceiver transferReceiver = new BroadcastReceiver() {

        @Override
        public void onReceive(Context context, Intent intent) {
            if (mTransferService == null) {
                return;
            }

            String type = intent.getStringExtra("type");
            if (type == null) {
                return;
            }

            if (type.equals(DownloadTaskManager.BROADCAST_FILE_DOWNLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                DownloadTaskInfo info = mTransferService.getDownloadTaskInfo(taskID);
                if (info != null) {
                    if (monitor.isStarted()) {
                        monitor.onFileDownloaded(info.account, info.repoID, info.repoName,
                                info.pathInRepo, info.localFilePath);
                    }
                }
            } else if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                UploadTaskInfo info = mTransferService.getUploadTaskInfo(taskID);

                if (info != null && info.isUpdate) {
                    updateMgr.onFileUpdateSuccess(info.account, info.repoID, info.repoName,
                            info.parentDir, info.localFilePath, info.version);
                }
            } else if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                UploadTaskInfo info = mTransferService.getUploadTaskInfo(taskID);

                if (info != null && info.isUpdate) {
                    updateMgr.onFileUpdateFailure(info.account, info.repoID, info.repoName,
                            info.parentDir, info.localFilePath, info.err, info.version);
                }
            }

        }

    };
}
