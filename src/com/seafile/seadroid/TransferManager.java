package com.seafile.seadroid;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

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

    @SuppressWarnings("unused")
    private static final String DEBUG_TAG = "TransferManager";

    public enum TaskState { INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED }

    public interface TransferListener {

        public void onFileUploadProgress(int taskID);
        public void onFileUploaded(int taskID);
        public void onFileUploadCancelled(int taskID);
        public void onFileUploadFailed(int taskID);

        public void onFileDownloaded(String repoID, String path, String fileID);
        public void onFileDownloadFailed(String repoName, String repoID, String path, String fileID,
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

    /**
     * Add a new upload task
     * @return Return false if there is a duplicating task, otherwise return true
     */
    public boolean addUploadTask(Account account, String repoID, String repoName,
                              String dir, String filePath, boolean isUpdate) {
        Iterator<UploadTask> iter = uploadTasks.iterator();
        while (iter.hasNext()) {
            UploadTask task = iter.next();
            if (task.myRepoID.equals(repoID) && task.myPath.equals(filePath)) {
                if (task.myState == TaskState.CANCELLED || task.myState == TaskState.FAILED
                        || task.myState == TaskState.FINISHED) {
                    // If there is an duplicate, but it has failed or been
                    // cancelled, remove it first
                    iter.remove();
                    break;
                } else {
                    // An duplicate task is uploading
                    return false;
                }
            }
        }

        UploadTask task = new UploadTask(account, repoID, repoName, dir, filePath, isUpdate);
        task.execute();
        return true;
    }

    /**
     * Add a new download task
     * @return Return false if there is a duplicating task, otherwise return true
     */
    public boolean addDownloadTask(Account account, String repoName, String repoID, String path,
            String fileID, long size) {
        // Check duplication
        for (DownloadTask task : downloadTasks) {
            if (task.myRepoID.equals(repoID) && task.myPath.equals(path)) {
                return false;
            }
        }
        DownloadTask task = new DownloadTask(account, repoName, repoID, path, fileID, size);
        task.execute();
        return true;
    }

    private UploadTask getUploadTaskByID(int taskID) {
        for (UploadTask task : uploadTasks) {
            if (task.getTaskID() == taskID) {
                return task;
            }
        }
        return null;
    }

    public UploadTaskInfo getUploadTaskInfo (int taskID) {
        UploadTask task = getUploadTaskByID(taskID);
        if (task != null) {
            return task.getTaskInfo();
        }

        return null;
    }

    public List<UploadTaskInfo> getAllUploadTaskInfos() {
        ArrayList<UploadTaskInfo> infos = new ArrayList<UploadTaskInfo>();
        for (UploadTask task : uploadTasks) {
            infos.add(task.getTaskInfo());
        }

        return infos;
    }

    public void removeUploadTask(int taskID) {
        UploadTask task = getUploadTaskByID(taskID);
        if (task != null) {
            uploadTasks.remove(task);
        }
    }

    public void removeFinishedUploadTasks() {
        Iterator<UploadTask> iter = uploadTasks.iterator();
        while (iter.hasNext()) {
            UploadTask task = iter.next();
            if (task.getState() == TaskState.FINISHED) {
                iter.remove();
            }
        }
    }

    public void cancelUploadTask(int taskID) {
        UploadTask task = getUploadTaskByID(taskID);
        if (task != null) {
            task.cancelUpload();
        }
    }

    public void retryUploadTask(int taskID) {
        UploadTask task = getUploadTaskByID(taskID);
        if (task != null) {
            task.retryUpload();
        }
    }

    private class UploadTask extends AsyncTask<String, Long, String> {

        private String myRepoID;
        private String myRepoName;
        private String myDir;   // parent dir
        private String myPath;  // local file path
        private boolean isUpdate;  // true if update an existing file

        private TaskState myState;
        private int myID;
        private long myUploaded;
        private long mySize;

        private String newFileID; // Only used in update tasks;

        SeafException err;

        Account account;

        public UploadTask(Account account, String repoID, String repoName,
                          String dir, String filePath, boolean isUpdate) {
            this.account = account;
            this.myRepoID = repoID;
            this.myRepoName = repoName;
            this.myDir = dir;
            this.myPath = filePath;
            this.isUpdate = isUpdate;
            this.newFileID = null;

            File f = new File(filePath);
            mySize = f.length();

            myID = ++notificationID;
            myState = TaskState.INIT;
            myUploaded = 0;

            // Log.d(DEBUG_TAG, "stored object is " + myPath + myObjectID);
            uploadTasks.add(this);
            err = null;
        }

        public int getTaskID() {
            return myID;
        }

        public TaskState getState() {
            return myState;
        }

        public UploadTaskInfo getTaskInfo() {
            UploadTaskInfo info = new UploadTaskInfo(myID, myState, myRepoID,
                                                     myRepoName, myDir, myPath, isUpdate,
                                                     myUploaded, mySize, newFileID, err);
            return info;
        }

        public void retryUpload() {
            if (myState != TaskState.CANCELLED && myState != TaskState.FAILED) {
                return;
            }
            uploadTasks.remove(this);
            addUploadTask(account, myRepoID, myRepoName, myDir, myPath, isUpdate);
        }

        public void cancelUpload() {
            if (myState != TaskState.INIT && myState != TaskState.TRANSFERRING) {
                return;
            }
            myState = TaskState.CANCELLED;
            super.cancel(true);
        }

        @Override
        protected void onPreExecute() {
            myState = TaskState.TRANSFERRING;
        }

        @Override
        protected void onProgressUpdate(Long... values) {
            long uploaded = values[0];
            myUploaded = uploaded;
            listener.onFileUploadProgress(myID);
        }

        @Override
        protected String doInBackground(String... params) {
            try {
                DataManager dataManager = new DataManager(account);
                ProgressMonitor monitor = new ProgressMonitor() {
                    @Override
                    public void onProgressNotify(long uploaded) {
                        publishProgress(uploaded);
                    }

                    @Override
                    public boolean isCancelled() {
                        return UploadTask.this.isCancelled();
                    }
                };
                if (isUpdate) {
                    return dataManager.updateFile(myRepoID, myDir, myPath, monitor);
                } else {
                    dataManager.uploadFile(myRepoID, myDir, myPath, monitor);
                    return null;
                }
            } catch (SeafException e) {
                Log.d("Upload", "Exception " + e.getCode() + " " + e.getMessage());
                err = e;
            }
            return null;
        }

        @Override
        protected void onPostExecute(String newFileID) {
            this.newFileID = newFileID;
            myState = err == null ? TaskState.FINISHED : TaskState.FAILED;
            if (listener != null) {
                if (err == null) {
                    listener.onFileUploaded(myID);
                }
                else {
                    listener.onFileUploadFailed(myID);
                }
            }
        }

        @Override
        protected void onCancelled() {
            if (listener != null) {
                listener.onFileUploadCancelled(myID);
            }
        }
    }

    private class DownloadTask extends AsyncTask<String, Integer, File> {

        Notification notification;
        NotificationManager notificationManager;
        private int showProgressThreshold = 1024 * 100; // 100KB
        private int myNtID;

        Account account;
        private String myRepoName;
        private String myRepoID;
        private String myPath;
        private String myFileID;
        private long mySize;
        SeafException err;

        public DownloadTask(Account account, String repoName, String repoID, String path,
                String fileID, long size) {
            this.account = account;
            this.myRepoName = repoName;
            this.myRepoID = repoID;
            this.myPath = path;
            this.myFileID = fileID;
            this.mySize = size;
            // Log.d(DEBUG_TAG, "stored object is " + myPath + myObjectID);
            downloadTasks.add(this);
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
                    return dataManager.getFile(myRepoName, myRepoID, myPath, myFileID, null);
                else
                    return dataManager.getFile(myRepoName, myRepoID, myPath, myFileID,
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
                    listener.onFileDownloadFailed(myRepoName, myRepoID, myPath, myFileID, mySize, err);
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

    public class UploadTaskInfo {
        public final int taskID;
        public final TaskState state;
        public final String repoID;
        public final String repoName;
        public final String parentDir;
        public final String localFilePath;
        public final boolean isUpdate;
        public final long uploadedSize, totalSize;
        public final String newFileID; // only used for update tasks
        public final SeafException err;

        public UploadTaskInfo(int taskID, TaskState state, String repoID,
                              String repoName, String parentDir,
                              String localFilePath, boolean isUpdate,
                              long uploadedSize, long totalSize, String newFileID,
                              SeafException err) {
            this.taskID = taskID;
            this.state = state;
            this.repoID = repoID;
            this.repoName = repoName;
            this.parentDir = parentDir;
            this.localFilePath = localFilePath;
            this.isUpdate = isUpdate;
            this.uploadedSize = uploadedSize;
            this.totalSize = totalSize;
            this.newFileID = newFileID;
            this.err = err;
        }
    }
}
