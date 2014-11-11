package com.seafile.seadroid2.transfer;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import android.os.AsyncTask;
import android.util.Log;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ProgressMonitor;

/**
 * Manages file downloading and uploading.
 *
 * Currently use an AsyncTask for an file.
 */
public class TransferManager {
    private static final String DEBUG_TAG = "TransferManager";

    public enum TaskState { INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED }

    public interface TransferListener {
        void onFileUploadProgress(int taskID);
        void onFileUploaded(int taskID);
        void onFileUploadCancelled(int taskID);
        void onFileUploadFailed(int taskID);

        void onFileDownloadProgress(int taskID);
        void onFileDownloaded(int taskID);
        void onFileDownloadFailed(int taskID);
    }

    private ArrayList<UploadTask> uploadTasks;
    private ArrayList<DownloadTask> downloadTasks;
    private int notificationID;
    TransferListener listener;

    public TransferManager() {
        notificationID = 0;
        uploadTasks = Lists.newArrayList();
        downloadTasks = Lists.newArrayList();
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
                              String dir, String filePath, boolean isUpdate, boolean isCopyToLocal) {
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

        UploadTask task = new UploadTask(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal);
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
        ArrayList<UploadTaskInfo> infos = Lists.newArrayList();
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
    
    private class UploadTask extends AsyncTask<String, Long, Void> {
        private String myRepoID;
        private String myRepoName;
        private String myDir;   // parent dir
        private String myPath;  // local file path
        private boolean isUpdate;  // true if update an existing file
        private boolean isCopyToLocal; // false to turn off copy operation
        
        private TaskState myState;
        private int myID;
        private long myUploaded;
        private long mySize;
        private DataManager dataManager;

        SeafException err;

        Account account;

        public UploadTask(Account account, String repoID, String repoName,
                          String dir, String filePath, boolean isUpdate, boolean isCopyToLocal) {
            this.account = account;
            this.myRepoID = repoID;
            this.myRepoName = repoName;
            this.myDir = dir;
            this.myPath = filePath;
            this.isUpdate = isUpdate;
            this.isCopyToLocal = isCopyToLocal;
            this.dataManager = new DataManager(account);

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
            UploadTaskInfo info = new UploadTaskInfo(myID, account, myState, myRepoID,
                                                     myRepoName, myDir, myPath, isUpdate, isCopyToLocal,
                                                     myUploaded, mySize, err);
            return info;
        }

        public void retryUpload() {
            if (myState != TaskState.CANCELLED && myState != TaskState.FAILED) {
                return;
            }
            uploadTasks.remove(this);
            addUploadTask(account, myRepoID, myRepoName, myDir, myPath, isUpdate, isCopyToLocal);
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
            // Log.d(DEBUG_TAG, "Uploaded " + uploaded);
            myUploaded = uploaded;
            listener.onFileUploadProgress(myID);
        }

        @Override
        protected Void doInBackground(String... params) {
            try {
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
                    dataManager.updateFile(myRepoName, myRepoID, myDir, myPath, monitor, isCopyToLocal);
                } else {
                    Log.d(DEBUG_TAG, "Upload path: " + myPath);
                    dataManager.uploadFile(myRepoName, myRepoID, myDir, myPath, monitor, isCopyToLocal);
                }
            } catch (SeafException e) {
                Log.d(DEBUG_TAG, "Upload exception " + e.getCode() + " " + e.getMessage());
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
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
        private String myPath, myLocalPath;
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
                    myLocalPath = file.getPath();
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
            DownloadTaskInfo info = new DownloadTaskInfo(account, taskID, myState, myRepoID,
                                                         myRepoName, myPath, myLocalPath, mySize, finished, err);
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
}
