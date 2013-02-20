package com.seafile.seadroid;

import java.util.List;

import com.seafile.seadroid.TransferManager.TransferListener;
import com.seafile.seadroid.TransferManager.UploadTaskInfo;
import com.seafile.seadroid.account.Account;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

public class TransferService extends Service implements TransferListener {

    private static final String DEBUG_TAG = "TransferService";

    public static final String BROADCAST_ACTION =
            "com.seafile.seadroid.TX_BROADCAST";

    private final IBinder mBinder = new TransferBinder();
    private TransferManager txManager;

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

    public void addUploadTask(Account account, String repoID, String repoName, String dir,
            String filePath) {
        txManager.addUploadTask(account, repoID, repoName, dir, filePath);
    }

    public void addDownloadTask(Account account, String repoID, String path,
            String fileID, long size) {
        txManager.addDownloadTask(account, repoID, path, fileID, size);
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

    @Override
    public void onFileUploadProgress(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", "uploadProgress")
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploaded(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", "uploaded")
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadCancelled(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", "uploadCancelled")
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

    @Override
    public void onFileUploadFailed(int taskID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", "uploadFailed")
            .putExtra("taskID", taskID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }
    @Override
    public void onFileDownloaded(String repoID, String path, String fileID) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", "downloaded")
                .putExtra("repoID", repoID).putExtra("path", path).putExtra("fileID", fileID);
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }


    @Override
    public void onFileDownloadFailed(String repoID, String path, String fileID,
            long size, SeafException err) {
        Intent localIntent = new Intent(BROADCAST_ACTION).putExtra("type", "downloadFailed")
                .putExtra("repoID", repoID).putExtra("path", path).putExtra("fileID", fileID)
                .putExtra("size", size).putExtra("errCode", err.getCode())
                .putExtra("errMsg", err.getMessage());
        LocalBroadcastManager.getInstance(this).sendBroadcast(localIntent);
    }

}
