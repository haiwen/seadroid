package com.seafile.seadroid2.transfer;

import android.os.AsyncTask;
import android.util.Log;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ProgressMonitor;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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

    private List<UploadTask> uploadWaitingList = Lists.newArrayList();
    private List<UploadTask> uploadingList = Lists.newArrayList();
    private static final int UPLOAD_MAX_COUNT = 2;

    private List<DownloadTask> downloadingList = Lists.newArrayList();
    private List<DownloadTask> downloadWaitingList = Lists.newArrayList();
    private static final int DOWNLOAD_MAX_COUNT = 2;

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
     * this method is deprecated.
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
     * add upload task to queue
     * @param account
     * @param repoID
     * @param repoName
     * @param dir
     * @param filePath
     * @param isUpdate
     * @param isCopyToLocal
     */
    public int addTaskToUploadQue(Account account,
                                   String repoID,
                                   String repoName,
                                   String dir,
                                   String filePath,
                                   boolean isUpdate,
                                   boolean isCopyToLocal) {

        int taskID = -1;
        synchronized (uploadWaitingList) {
            boolean hasInQue = false;
            for (UploadTask uploadTask : uploadWaitingList) {
                if (uploadTask.myPath.equals(filePath)) {
                    hasInQue = true;
                    taskID = uploadTask.getTaskID();
                    // Log.d(DEBUG_TAG, "in  Que  " + index + " " + filePath + "in waiting list");
                    break;
                }
            }

            for (UploadTask uploadTask : uploadingList) {
                if (uploadTask.myPath.equals(filePath)) {
                    hasInQue = true;
                    taskID = uploadTask.getTaskID();
                    // Log.d(DEBUG_TAG, "in  Que  " + index + " " + filePath + "in uploading list");
                    break;
                }
            }

            if (!hasInQue) {
                UploadTask task = new UploadTask(account, repoID, repoName, dir, filePath, isUpdate, isCopyToLocal);
                // Log.d(DEBUG_TAG, "add Que  " + task.getTaskID() + " " + filePath);
                taskID = task.getTaskID();

                uploadWaitingList.add(task);

                uploadNext();
            }
        }
        return taskID;
    }

    private void uploadNextInQue(int taskID) {
        synchronized (uploadWaitingList) {
            removeCurrentUploadItem(taskID);
            uploadNext();
        }
    }

    public void uploadNext() {
        // Log.d(DEBUG_TAG, "UploadingCount:" + uploadingList.size());
        if (!uploadWaitingList.isEmpty()
                && uploadingList.size() < UPLOAD_MAX_COUNT) {
            // Log.d(DEBUG_TAG, "do next!");

            UploadTask item = uploadWaitingList.get(0);
            uploadingList.add(item);
            uploadWaitingList.remove(0);
            ConcurrentAsyncTask.execute(item);
        }
    }

    private void removeCurrentUploadItem(int taskID) {
        // Log.d(DEBUG_TAG, "removeCurrentUploadItem:" + taskID);
        if (!uploadingList.isEmpty()) {
            UploadTask toRemove = getUploadTaskByID(taskID);
            for (int i = 0; i < uploadingList.size(); i++) {
                if (uploadingList.get(i).myPath.equals(toRemove.myPath)) {
                    // Log.d(DEBUG_TAG, "Done Que " + taskID + " " + uploadingList.get(i).myPath);
                    uploadingList.remove(i);
                    break;
                }
            }
        }

    }

    /**
     * Add a new download task
     * this method is deprecated.
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

    public int addTaskToDownloadQue(Account account,
                                     String repoName,
                                     String repoID,
                                     String path) {
        int taskID = -1;
        synchronized (downloadWaitingList) {
            boolean hasInQue = false;
            for (DownloadTask downloadTask : downloadWaitingList) {
                if (downloadTask.myRepoName.equals(repoName) &&
                        downloadTask.myPath.equals(path)) {
                    hasInQue = true;
                    taskID = downloadTask.getTaskID();
                    // Log.d(DEBUG_TAG, "in  Que  " + index + " " + repoName + path + "in waiting list");
                    break;
                }
            }

            for (DownloadTask downloadTask : downloadingList) {
                if (downloadTask.myRepoName.equals(repoName) &&
                        downloadTask.myPath.equals(path)) {
                    hasInQue = true;
                    taskID = downloadTask.getTaskID();
                    // Log.d(DEBUG_TAG, "in  Que  " + index + " " + repoName + path + " in downloading list" );
                    break;
                }
            }

            if (!hasInQue) {
                DownloadTask task = new DownloadTask(account, repoName, repoID, path);

                taskID = task.getTaskID();
                // Log.d(DEBUG_TAG, "add Que  " + task.getTaskID() + " " + repoName + path);
                downloadWaitingList.add(task);
                downloadNext();
            }
        }
        return taskID;
    }

    public void downloadNext() {
        // Log.d(DEBUG_TAG, "DownloadingCount:" + downloadWaitingList.size());
        if (!downloadWaitingList.isEmpty()
                && downloadingList.size() < DOWNLOAD_MAX_COUNT) {
            // Log.d(DEBUG_TAG, "do next!");

            DownloadTask item = downloadWaitingList.get(0);
            downloadingList.add(item);
            downloadWaitingList.remove(0);
            ConcurrentAsyncTask.execute(item);
        }
    }

    private void downloadNextInQue(int taskID) {
        synchronized (downloadWaitingList) {
            removeCurrentDownloadItem(taskID);
            downloadNext();
        }
    }

    private void removeCurrentDownloadItem(int taskID) {
        // Log.d(DEBUG_TAG, "removeCurrentDownloadItem:" + taskID);

        if (!downloadingList.isEmpty()) {
            DownloadTask toRemove = getDownloadTaskByID(taskID);
            for (int i = 0; i < downloadingList.size(); i++) {
                if (downloadingList.get(i).myRepoName.equals(toRemove.myRepoName) &&
                        downloadingList.get(i).myPath.equals(toRemove.myPath)) {
                    // Log.d(DEBUG_TAG, "Done Que " + taskID + " " + toRemove.myRepoName + toRemove.myPath);
                    downloadingList.remove(i);
                    break;
                }
            }
        }

    }

    public void removeCancelledDownloadItemInQue(int taskID) {

        synchronized (downloadWaitingList) {

            DownloadTask toCancel = getDownloadTaskByID(taskID);
            if (!downloadWaitingList.isEmpty()) {
                for (int i = 0; i < downloadWaitingList.size(); i++) {
                    if (downloadWaitingList.get(i).myRepoName.equals(toCancel.myRepoName) &&
                            downloadWaitingList.get(i).myPath.equals(toCancel.myPath)) {
                        // Log.d(DEBUG_TAG, "CancelQue " + taskID + " " + toRemove.myRepoName + toRemove.myPath + " in waiting list");
                        downloadWaitingList.remove(i);
                        break;
                    }
                }
            }

            if (!downloadingList.isEmpty()) {
                for (int i = 0; i< downloadingList.size(); i++) {
                    if (downloadingList.get(i).myRepoName.equals(toCancel.myRepoName) &&
                            downloadingList.get(i).myPath.equals(toCancel.myPath)) {
                        // Log.d(DEBUG_TAG, "CancelQue " + taskID + " " + toRemove.myRepoName + toRemove.myPath + " in downloading list");
                        downloadingList.remove(i);
                        break;
                    }
                }
            }
        }

    }

    public void removeCancelledUploadItemInQue(int taskID) {

        synchronized (uploadWaitingList) {

            UploadTask toCancel = getUploadTaskByID(taskID);

            if (!uploadWaitingList.isEmpty()) {
                for (int i = 0; i < uploadWaitingList.size(); i++) {
                    if (uploadWaitingList.get(i).myPath.equals(toCancel.myPath)) {
                        // Log.d(DEBUG_TAG, "CancelQue " + taskID + " " + toCancel.myPath + " in waiting list");
                        uploadWaitingList.remove(i);
                        break;
                    }
                }
            }

            if (!uploadingList.isEmpty()) {
                for (int i = 0; i< uploadingList.size(); i++) {
                    if (uploadingList.get(i).myPath.equals(toCancel.myPath)) {
                        // Log.d(DEBUG_TAG, "CancelQue " + taskID + " " + toCancel.myPath + " in uploading list");
                        uploadingList.remove(i);
                        break;
                    }
                }
            }
        }

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

    public List<DownloadTaskInfo> getAllDownloadTaskInfos() {
        ArrayList<DownloadTaskInfo> infos = Lists.newArrayList();
        for (DownloadTask task : downloadTasks) {
            infos.add(task.getTaskInfo());
        }

        return infos;
    }

    /**
     * get all download task info under a specific directory.
     *
     * @param repoID
     * @param dir valid dir should be something like this "/DIRNAME/", instead of "/DIRNAME",
     *            in order to ensure the param being consistent with its caller
     * @return List<DownloadTaskInfo>
     */
    public List<DownloadTaskInfo> getDownloadTaskInfosByPath(String repoID, String dir) {
        ArrayList<DownloadTaskInfo> infos = Lists.newArrayList();
        for (DownloadTask task : downloadTasks) {
            String parentDir = Utils.getParentPath(task.myPath);
            String validDir;

            if (!parentDir.equals("/"))
                validDir = parentDir + "/";
            else
                validDir = parentDir;

            if (task.myRepoID.equals(repoID) && validDir.equals(dir))
                infos.add(task.getTaskInfo());
        }

        return infos;
    }

    public boolean isDownloading() {
        if (downloadTasks.isEmpty()) {
            return false;
        }

        for (DownloadTask task : downloadTasks) {
            if (task.getState().equals(TaskState.TRANSFERRING) || task.getState().equals(TaskState.INIT)) {
                return true;
            }
        }

        return false;
    }

    public boolean isUploading() {
        if (uploadTasks.isEmpty())
            return false;

        for (UploadTask task : uploadTasks) {
            if (task.getState().equals(TaskState.INIT) || task.getState().equals(TaskState.TRANSFERRING)) {
                return true;
            }
        }

        return false;
    }

    public void removeUploadTask(int taskID) {
        UploadTask task = getUploadTaskByID(taskID);
        if (task != null) {
            uploadTasks.remove(task);
        }
    }

    public void removeDownloadTask(int taskID) {
        DownloadTask task = getDownloadTaskByID(taskID);
        if (task != null) {
            downloadTasks.remove(task);
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

    public void removeAllDownloadTasksByState(TaskState taskState) {
        Iterator<DownloadTask> iter = downloadTasks.iterator();
        while (iter.hasNext()) {
            DownloadTask task = iter.next();
            if (task.getState().equals(taskState)) {
                iter.remove();
            }
        }
    }

    public void removeAllUploadTasksByState(TaskState taskState) {
        Iterator<UploadTask> iter = uploadTasks.iterator();
        while (iter.hasNext()) {
            UploadTask task = iter.next();
            if (task.getState().equals(taskState)) {
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

    public void retryDownloadTask(int taskID) {
        DownloadTask task = getDownloadTaskByID(taskID);
        if (task != null) {
            task.retryDownload();
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
            UploadTaskInfo info = new UploadTaskInfo(account, myID, myState, myRepoID,
                                                     myRepoName, myDir, myPath, isUpdate, isCopyToLocal,
                                                     myUploaded, mySize, err);
            return info;
        }

        public void retryUpload() {
            if (myState != TaskState.CANCELLED && myState != TaskState.FAILED) {
                return;
            }
            uploadTasks.remove(this);
            addTaskToUploadQue(account, myRepoID, myRepoName, myDir, myPath, isUpdate, isCopyToLocal);
        }

        public void cancelUpload() {
            if (myState != TaskState.INIT && myState != TaskState.TRANSFERRING) {
                return;
            }
            myState = TaskState.CANCELLED;

            removeCancelledUploadItemInQue(myID);
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
                    // Log.d(DEBUG_TAG, "Upload path: " + myPath);
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

            uploadNextInQue(myID);
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

            downloadNextInQue(taskID);
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
            removeCancelledDownloadItemInQue(taskID);
            super.cancel(true);
        }

        public void retryDownload() {
            if (myState != TaskState.CANCELLED && myState != TaskState.FAILED) {
                return;
            }
            downloadTasks.remove(this);
            addTaskToDownloadQue(account, myRepoName, myRepoID, myPath);
        }

        public TaskState getState() {
            return myState;
        }
    }
}
