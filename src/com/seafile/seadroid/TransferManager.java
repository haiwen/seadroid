package com.seafile.seadroid;

import java.io.File;
import java.util.ArrayList;

import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.DataManager.ProgressMonitor;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.util.Log;
import android.widget.RemoteViews;

/**
 * Manages file downloading and uploading.
 * 
 * Currently use an AsyncTask for an file.
 */
public class TransferManager {
    
    private static final String DEBUG_TAG = "TransferManager";
    
    public interface TransferListener {
        
        public void onFileUploaded(String repoID, String dir, String filePath);

        public void onFileUploadFailed(String repoID, String dir, String filePath,
                SeafException err);
        
        public void onFileDownloaded(String repoID, String path, String fileID);
        
        public void onFileDownloadFailed(String repoID, String path, String fileID,
                long size, SeafException err);
        
    }
    
    private ArrayList<UploadTask> uploadTasks;
    private ArrayList<DownloadTask> downloadTasks;
    private int notificationID;
    TransferListener listener;
    
    public TransferManager() {
        notificationID = 0;
        uploadTasks = new ArrayList<UploadTask>();
        downloadTasks = new ArrayList<DownloadTask>();
        listener = null;
    }
    
    public void setListener(TransferListener listener) {
        this.listener = listener;
    }
    
    public void unsetListener() {
        listener = null;
    }

    public void addUploadTask(Account account, String repoID, String dir, 
            String filePath) {
        UploadTask task = new UploadTask(account, repoID, dir, filePath);
        task.execute();
    }
    
    public void addDownloadTask(Account account, String repoID, String path, 
            String fileID, long size) {
        // check duplication
        for (DownloadTask task : downloadTasks) {
            if (task.myFileID.equals(fileID))
                return;
        }
        DownloadTask task = new DownloadTask(account, repoID, path, fileID, size);
        task.execute();
    }
    
    private class UploadTask extends AsyncTask<String, Integer, Void> {

        Notification notification;
        NotificationManager notificationManager;
        private int showProgressThreshold = 1024 * 100; // 100KB
        private int myNtID;
        
        private String myRepoID;
        private String myDir;
        private String myPath;
        long mySize;
        SeafException err;
        
        Account account;
        
        public UploadTask(Account account, String repoID, String dir, 
                String filePath) {
            this.account = account;
            this.myRepoID = repoID;
            this.myDir = dir;
            this.myPath = filePath;
            File f = new File(filePath);
            mySize = f.length();
            
            // Log.d(DEBUG_TAG, "stored object is " + myPath + myObjectID);
            uploadTasks.add(this);
            err = null;
        }

        @Override
        protected void onPreExecute() {
            if (mySize <= showProgressThreshold)
                return;
            myNtID = ++notificationID;
            Context context =  SeadroidApplication.getAppContext();
            notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
            Intent notificationIntent = new Intent(context, 
                    BrowserActivity.class);
            
            notificationIntent.putExtra("tab", "upload");

            PendingIntent intent = PendingIntent.getActivity(context, myNtID, notificationIntent, 0);

            notification = new Notification(R.drawable.ic_stat_upload, "", System.currentTimeMillis());
            notification.flags = notification.flags | Notification.FLAG_ONGOING_EVENT;
            notification.contentView = new RemoteViews(context.getPackageName(),
                    R.layout.download_progress);
            notification.contentView.setCharSequence(R.id.tv_download_title, "setText",
                    Utils.fileNameFromPath(myPath));
            notification.contentIntent = intent;
            
            notification.contentView.setProgressBar(R.id.pb_download_progressbar,
                    (int)mySize, 0, false);
            notificationManager.notify(myNtID, notification);
        }
        
        @Override
        protected void onProgressUpdate(Integer... values) {
            int progress = values[0];
            notification.contentView.setProgressBar(R.id.pb_download_progressbar,
                    (int)mySize, progress, false);
            notificationManager.notify(myNtID, notification);
        }

        @Override
        protected Void doInBackground(String... params) {

            try {
                DataManager dataManager = new DataManager(account);
                if (mySize <= showProgressThreshold)
                    dataManager.uploadFile(myRepoID, myDir, myPath, null);
                else
                    dataManager.uploadFile(myRepoID, myDir, myPath,
                            new ProgressMonitor() {

                                @Override
                                public void onProgressNotify(long total) {
                                    publishProgress((int)total);
                                }

                                @Override
                                public boolean isCancelled() {
                                    return UploadTask.this.isCancelled();
                                }
                            }
                    );
            } catch (SeafException e) {
                err = e;
            }
            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            uploadTasks.remove(this);
            if (listener != null) {
                if (err == null)
                    listener.onFileUploaded(myRepoID, myDir, myPath);
                else
                    listener.onFileUploadFailed(myRepoID, myDir, myPath, err);
            }
        }
        
        @Override
        protected void onCancelled() {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            uploadTasks.remove(this);
        }
    }
    
    private class DownloadTask extends AsyncTask<String, Integer, File> {

        Notification notification;
        NotificationManager notificationManager;
        private int showProgressThreshold = 1024 * 100; // 100KB
        private int myNtID;
        
        Account account;
        private String myRepoID;
        private String myPath;
        private String myFileID;
        private long mySize;
        SeafException err;
        
        public DownloadTask(Account account, String repoID, String path, 
                String fileID, long size) {
            this.account = account;
            this.myRepoID = repoID;
            this.myPath = path;
            this.myFileID = fileID;
            this.mySize = size;
            // Log.d(DEBUG_TAG, "stored object is " + myPath + myObjectID);
            downloadTasks.add(this);
            err = null;
        }
        
        public String getFileID() {
            return myFileID;
        }
        
        @Override
        protected void onPreExecute() {
            if (mySize <= showProgressThreshold)
                return;
            myNtID = ++notificationID;
            
            Context context =  SeadroidApplication.getAppContext();
            notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
            Intent notificationIntent = new Intent(context, 
                    BrowserActivity.class);
               
            PendingIntent intent = PendingIntent.getActivity(context, myNtID, notificationIntent, 0);

            notification = new Notification(R.drawable.ic_stat_download, "", System.currentTimeMillis());
            notification.flags = notification.flags | Notification.FLAG_ONGOING_EVENT;
            notification.contentView = new RemoteViews(context.getPackageName(),
                    R.layout.download_progress);
            notification.contentView.setCharSequence(R.id.tv_download_title, "setText",
                    Utils.fileNameFromPath(myPath));
            notification.contentIntent = intent;
            
            notification.contentView.setProgressBar(R.id.pb_download_progressbar,
                    (int)mySize, 0, false);
            notificationManager.notify(myNtID, notification);
        }
        
        @Override
        protected void onProgressUpdate(Integer... values) {
            int progress = values[0];
            notification.contentView.setProgressBar(R.id.pb_download_progressbar,
                    (int)mySize, progress, false);
            notificationManager.notify(myNtID, notification);
        }

        @Override
        protected File doInBackground(String... params) {
            try {
                DataManager dataManager = new DataManager(account);
                if (mySize <= showProgressThreshold)
                    return dataManager.getFile(myRepoID, myPath, myFileID, null);
                else
                    return dataManager.getFile(myRepoID, myPath, myFileID,
                            new ProgressMonitor() {

                                @Override
                                public void onProgressNotify(long total) {
                                    publishProgress((int) total);
                                }

                                @Override
                                public boolean isCancelled() {
                                    return DownloadTask.this.isCancelled();
                                }
                            }
                            );
            } catch (SeafException e) {
                err = e;
                return null;
            }
        }

        @Override
        protected void onPostExecute(File file) {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            downloadTasks.remove(this);
            
            if (listener != null) {
                if (file != null)
                    listener.onFileDownloaded(myRepoID, myPath, myFileID);
                else {
                    if (err == null)
                        err = SeafException.unknownException;
                    listener.onFileDownloadFailed(myRepoID, myPath, myFileID, mySize, err);
                }
            }
        }
        
        @Override
        protected void onCancelled() {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            downloadTasks.remove(this);
        }

    }
    
}
