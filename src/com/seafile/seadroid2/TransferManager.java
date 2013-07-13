package com.seafile.seadroid2;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.util.Log;
import android.widget.RemoteViews;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DataManager.ProgressMonitor;

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

        public void onFileDownloadProgress(int taskID);
        public void onFileDownloaded(int taskID);
        public void onFileDownloadFailed(int taskID);

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
     */
    public int addUploadTask(Account account, String repoID, String repoName,
                              String dir, String filePath, boolean isUpdate) {
        Iterator<UploadTask> iter = uploadTasks.iterator();
        while (iter.hasNext()) {
            UploadTask task = iter.next();
            if (task.myRepoID.equals(repoID) && task.myPath.equals(filePath)) {
                if (task.myState == TaskState.CANCELLED || task.myState == TaskState.FAILED
                        || task.myState == TaskState.FINISHED) {
                    // If there is a duplicate, but it has failed or been
                    // cancelled, remove it first
                    iter.remove();
                    break;
                } else {
                    // A duplicate task is uploading
                    return task.getTaskID();
                }
            }
        }

        UploadTask task = new UploadTask(account, repoID, repoName, dir, filePath, isUpdate);
        task.execute();
        return task.getTaskID();
    }

    /**
     * Add a new download task
     */
    public int addDownloadTask(Account account,
                               String repoName,
                               String repoID,
                               String path) {
        Iterator<DownloadTask> iter = downloadTasks.iterator();
        while (iter.hasNext()) {
            DownloadTask task = iter.next();
            if (task.myRepoID.equals(repoID) && task.myPath.equals(path)) {
                if (task.myState == TaskState.CANCELLED || task.myState == TaskState.FAILED
                        || task.myState == TaskState.FINISHED) {
                    // If there is a duplicate, but it has failed or been
                    // cancelled, remove it first
                    iter.remove();
                    break;
                } else {
                    // A duplicate task is downloading
                    return task.getTaskID();
                }
            }
        }

        DownloadTask task = new DownloadTask(account, repoName, repoID, path);
        task.execute();
        return task.getTaskID();
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

    public void cancelDownloadTask(int taskID) {
        DownloadTask task = getDownloadTaskByID(taskID);
        if (task != null) {
            task.cancelDownload();
        }
    }

    public void retryUploadTask(int taskID) {
        UploadTask task = getUploadTaskByID(taskID);
        if (task != null) {
            task.retryUpload();
        }
    }

    private DownloadTask getDownloadTaskByID(int taskID) {
        for (DownloadTask task : downloadTasks) {
            if (task.getTaskID() == taskID) {
                return task;
            }
        }
        return null;
    }

    public DownloadTaskInfo getDownloadTaskInfo (int taskID) {
        DownloadTask task = getDownloadTaskByID(taskID);
        if (task != null) {
            return task.getTaskInfo();
        }

        return null;
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
            Log.d(DEBUG_TAG, "Uploaded " + uploaded);
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

    private class DownloadTask extends AsyncTask<String, Long, File> {

        private int taskID;

        Account account;
        private String myRepoName;
        private String myRepoID;
        private String myPath;
        private long mySize, finished;
        private TaskState myState;
        SeafException err;

        public DownloadTask(Account account, String repoName, String repoID, String path) {
            this.account = account;
            this.myRepoName = repoName;
            this.myRepoID = repoID;
            this.myPath = path;
            this.myState = TaskState.INIT;

            // The size of the file would be known in the first progress update
            this.mySize = -1;
            this.taskID = ++notificationID;

            // Log.d(DEBUG_TAG, "stored object is " + myPath + myObjectID);
            downloadTasks.add(this);
            err = null;
        }

        /**
         * When downloading a file, we don't know the file size in advance, so
         * we make use of the first progress update to return the file size.
         */
        @Override
        protected void onProgressUpdate(Long... values) {
            if (mySize == -1) {
                mySize = values[0];
                myState = TaskState.TRANSFERRING;
                return;
            }
            finished = values[0];
            listener.onFileDownloadProgress(taskID);
        }

        @Override
        protected File doInBackground(String... params) {
            try {
                DataManager dataManager = new DataManager(account);
                return dataManager.getFile(myRepoName, myRepoID, myPath,
                        new ProgressMonitor() {

                            @Override
                            public void onProgressNotify(long total) {
                                publishProgress(total);
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
            if (listener != null) {
                if (file != null) {
                    myState = TaskState.FINISHED;
                    listener.onFileDownloaded(taskID);
                } else {
                    myState = TaskState.FAILED;
                    if (err == null)
                        err = SeafException.unknownException;
                    listener.onFileDownloadFailed(taskID);
                }
            }
        }

        @Override
        protected void onCancelled() {
            myState = TaskState.CANCELLED;
        }

        public int getTaskID() {
            return taskID;
        }

        public DownloadTaskInfo getTaskInfo() {
            DownloadTaskInfo info = new DownloadTaskInfo(taskID, myState, myRepoID,
                                                         myRepoName, myPath, mySize, finished, err);
            return info;
        }

        public void cancelDownload() {
            if (myState != TaskState.INIT && myState != TaskState.TRANSFERRING) {
                return;
            }
            myState = TaskState.CANCELLED;
            super.cancel(true);
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

    public class DownloadTaskInfo {
        public final int taskID;
        public final TaskState state;
        public final String repoID;
        public final String repoName;
        public final String path;
        public final long fileSize, finished;
        public final SeafException err;

        public DownloadTaskInfo(int taskID, TaskState state, String repoID,
                                String repoName, String path,
                                long fileSize, long finished,
                                SeafException err) {
            this.taskID = taskID;
            this.state = state;
            this.repoID = repoID;
            this.repoName = repoName;
            this.path = path;
            this.fileSize = fileSize;
            this.finished = finished;
            this.err = err;
        }
    }
}
