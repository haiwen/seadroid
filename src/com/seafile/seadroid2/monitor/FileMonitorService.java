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
import com.seafile.seadroid2.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.TransferService;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.SeafCachedFile;

/**
 * Monitor changes of local cached files, and upload them through
 * TransferService if moidified
 */
public class FileMonitorService extends Service implements
		SeafileObserver.CachedFileChangedListener {

	private static final String DEBUG_TAG = "FileMonitorService";

	private SeafileMonitor fileMonitor;
	private TransferService mTransferService;
	private final IBinder mBinder = new MonitorBinder();

	private BroadcastReceiver downloadReceiver = new BroadcastReceiver() {

		@Override
		public void onReceive(Context context, Intent intent) {

			String type = intent.getStringExtra("type");
			if (type == null) {
				return;
			}

			if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_SUCCESS)) {

				int taskID = intent.getIntExtra("taskID", 0);
				DownloadTaskInfo info = mTransferService
						.getDownloadTaskInfo(taskID);
				if (info != null) {
					SeafCachedFile tmpCachedFile = new SeafCachedFile();
					tmpCachedFile.repoID = info.repoID;
					tmpCachedFile.repoName = info.repoName;
					tmpCachedFile.path = info.path;
					Account account = info.account;
					Log.d(DEBUG_TAG, account.email);
					fileMonitor.getObserverForAccount(account).addToMap(tmpCachedFile);
				}
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

		registerReceiver(downloadReceiver, new IntentFilter(
				TransferService.BROADCAST_ACTION));
	}

	@Override
	public void onDestroy() {
		Log.d(DEBUG_TAG, "onDestroy");
		unbindService(mTransferConnection);
		LocalBroadcastManager.getInstance(this).unregisterReceiver(
				downloadReceiver);

        if (fileMonitor != null) {
            try {
                fileMonitor.stop();
            } catch(Exception e) {
				Log.d(DEBUG_TAG, "failed to stop  file monitor");
            }
        }
	}

	public void addAccounts(List<Account> accounts) {
	}

	public void removeAccount(Account account) {
		Log.d(DEBUG_TAG, account.email);
		fileMonitor.stopMonitorFilesForAccount(account);
	}

	@Override
	public void onCachedFiledChanged(Account account,
			SeafCachedFile cachedFile, File localFile) {
		if (mTransferService != null) {
			mTransferService.addUploadTask(account, cachedFile.repoID,
					cachedFile.repoName, Utils.getParentPath(cachedFile.path),
					localFile.getPath(), true);
		}
	}

	private ServiceConnection mTransferConnection = new ServiceConnection() {

		@Override
		public void onServiceConnected(ComponentName className, IBinder binder) {
			TransferService.TransferBinder transferBinder = (TransferService.TransferBinder) binder;
			mTransferService = transferBinder.getService();
			LocalBroadcastManager.getInstance(FileMonitorService.this)
					.registerReceiver(downloadReceiver,
							new IntentFilter(TransferService.BROADCAST_ACTION));
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
			}

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
        }
    }
}
