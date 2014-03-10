package com.seafile.seadroid2.monitor;

import java.io.File;
import java.util.List;

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Binder;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.transfer.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TransferManager.UploadTaskInfo;
import com.seafile.seadroid2.transfer.TransferService;

/**
 * Monitor changes of local cached files, and upload them through TransferService if moidified
 */
public class FileMonitorService extends Service implements
        SeafileObserver.CachedFileChangedListener {

    private static final String DEBUG_TAG = "FileMonitorService";

    private SeafileMonitor fileMonitor;
    private TransferService mTransferService;
    private AutoUpdateManager updateMgr = new AutoUpdateManager();
    private final IBinder mBinder = new MonitorBinder();

    private BroadcastReceiver downloadReceiver = new BroadcastReceiver() {

        @Override
        public void onReceive(Context context, Intent intent) {
            if (mTransferService == null) {
                return;
            }

            String type = intent.getStringExtra("type");
            if (type == null) {
                return;
            }

            if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_SUCCESS)) {

                int taskID = intent.getIntExtra("taskID", 0);
                DownloadTaskInfo info = mTransferService.getDownloadTaskInfo(taskID);
                if (info != null) {
                    fileMonitor.onFilesDownloaded(info.account, info.repoID, info.repoName, info.pathInRepo, info.localPath);
                    Log.d(DEBUG_TAG, "start watch downloaded file " + info.pathInRepo);
                }
            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                UploadTaskInfo info = mTransferService.getUploadTaskInfo(taskID);

                updateMgr.onFileUpdateSuccess(info.account, info.repoID, info.repoName, info.parentDir,
                        info.localFilePath);
            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                UploadTaskInfo info = mTransferService.getUploadTaskInfo(taskID);

                updateMgr.onFileUpdateFailure(info.account, info.repoID, info.repoName, info.parentDir,
                                              info.localFilePath, info.err);
            }

        }

    };

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand called.");

        if (fileMonitor == null) {
            fileMonitor = new SeafileMonitor(this);
            ConcurrentAsyncTask.execute(new InitMonitorTask());
        }

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

        LocalBroadcastManager.getInstance(this).registerReceiver(downloadReceiver,
                new IntentFilter(TransferService.BROADCAST_ACTION));
    }

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");

        updateMgr.stop();

        if (fileMonitor != null) {
            try {
                fileMonitor.stop();
            } catch (Exception e) {
                Log.d(DEBUG_TAG, "failed to stop file monitor");
            }
        }

        if (mTransferService != null) {
            unbindService(mTransferConnection);
            mTransferService = null;
        }

        LocalBroadcastManager.getInstance(this).unregisterReceiver(downloadReceiver);
    }

    public void removeAccount(Account account) {
        Log.d(DEBUG_TAG, account.email);
        fileMonitor.stopMonitorFilesForAccount(account);
    }

    @Override
    public void onCachedFiledChanged(final Account account, final SeafCachedFile cachedFile,
            final File localFile) {

        updateMgr.addTask(account, cachedFile, localFile);

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

    private class InitMonitorTask extends AsyncTask<Void, Void, Void> {
        @Override
        protected Void doInBackground(Void... params) {
            List<Account> accounts = new AccountManager(FileMonitorService.this).getAccountList();

            for (Account account : accounts) {
                fileMonitor.monitorFilesForAccount(account);
            }

            try {
                fileMonitor.start();
            } catch (Exception e) {
                Log.w(DEBUG_TAG, "failed to start file monitor");
                throw new RuntimeException("failed to start file monitor");
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {}
    }
}
