package com.seafile.seadroid2;

import java.util.List;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;

import com.seafile.seadroid2.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.TransferManager.TransferListener;
import com.seafile.seadroid2.TransferManager.UploadTaskInfo;
import com.seafile.seadroid2.account.Account;

public class TransferService extends Service implements TransferListener {

    @SuppressWarnings("unused")
    private static final String DEBUG_TAG = "TransferService";

    public static final String BROADCAST_ACTION =
            "com.seafile.seadroid.TX_BROADCAST";

    private final IBinder mBinder = new TransferBinder();
    private TransferManager txManager;

    public static final String BROADCAST_FILE_DOWNLOAD_SUCCESS = "downloaded";
    public static final String BROADCAST_FILE_DOWNLOAD_FAILED = "downloadFailed";
    public static final String BROADCAST_FILE_DOWNLOAD_PROGRESS = "downloadProgress";

    public static final String BROADCAST_FILE_UPLOAD_SUCCESS = "uploaded";
    public static final String BROADCAST_FILE_UPLOAD_FAILED = "uploadFailed";
    public static final String BROADCAST_FILE_UPLOAD_PROGRESS = "uploadProgress";
    public static final String BROADCAST_FILE_UPLOAD_CANCELLED = "uploadCancelled";

    @Override
    public void onCreate() {
        txManager = new TransferManager();
        txManager.setListener(this);
    }

    @Override
    public void onDestroy() {
        txManager.unsetListener();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    public class TransferBinder extends Binder {
        public TransferService getService() {
            return TransferService.this;
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        //Log.d(DEBUG_TAG, "onBind");
        return mBinder;
    }

    public int addUploadTask(Account account, String repoID, String repoName, String dir,
                              String filePath, boolean isUpdate) {
        return txManager.addUploadTask(account, repoID, repoName, dir, filePath, isUpdate);
    }

    public int addDownloadTask(Account account,
                               String repoName,
                               String repoID,
                               String path) {
        return txManager.addDownloadTask(account, repoName, repoID, path);
    }

    public UploadTaskInfo getUploadTaskInfo(int taskID) {
        return txManager.getUploadTaskInfo(taskID);
    }

    public List<UploadTaskInfo> getAllUploadTaskInfos() {
        return txManager.getAllUploadTaskInfos();
    }

    public void removeUploadTask(int taskID) {
        txManager.removeUploadTask(taskID);
    }

    public void removeFinishedUploadTasks() {
        txManager.removeFinishedUploadTasks();
    }

    public void cancelUploadTask(int taskID) {
        txManager.cancelUploadTask(taskID);
    }

    public void retryUploadTask(int taskID) {
        txManager.retryUploadTask(taskID);
    }

    public DownloadTaskInfo getDownloadTaskInfo(int taskID) {
        return txManager.getDownloadTaskInfo(taskID);
    }

    @Override
    public void onFileUploadProgress(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_PROGRESS)
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploaded(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_SUCCESS)
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadCancelled(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_CANCELLED)
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadFailed(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_UPLOAD_FAILED)
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloadProgress(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_DOWNLOAD_PROGRESS)
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloaded(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_DOWNLOAD_SUCCESS)
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileDownloadFailed(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", BROADCAST_FILE_DOWNLOAD_FAILED)
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    public void cancelDownloadTask(int taskID) {
        txManager.cancelDownloadTask(taskID);
    }
}
