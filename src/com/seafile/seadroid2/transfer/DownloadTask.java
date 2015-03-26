package com.seafile.seadroid2.transfer;

import java.io.File;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ProgressMonitor;

/**
 * Download task
 *
 */
public class DownloadTask extends TransferTask {

    private String localPath;
    private DownloadStateListener downloadStateListener;

    public DownloadTask(int taskID, Account account, String repoName, String repoID, String path,
                        DownloadStateListener downloadStateListener) {
        super(taskID, account, repoName, repoID, path);
        this.downloadStateListener = downloadStateListener;
    }

    /**
     * When downloading a file, we don't know the file size in advance, so
     * we make use of the first progress update to return the file size.
     */
    @Override
    protected void onProgressUpdate(Long... values) {
        if (totalSize == -1) {
            totalSize = values[0];
            state = TaskState.TRANSFERRING;
            return;
        }
        finished = values[0];
        downloadStateListener.onFileDownloadProgress(taskID);
    }

    @Override
    protected File doInBackground(Void... params) {
        try {
            DataManager dataManager = new DataManager(account);
            return dataManager.getFile(repoName, repoID, path,
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
        if (downloadStateListener != null) {
            if (file != null) {
                state = TaskState.FINISHED;
                localPath = file.getPath();
                downloadStateListener.onFileDownloaded(taskID);
            } else {
                state = TaskState.FAILED;
                if (err == null)
                    err = SeafException.unknownException;
                downloadStateListener.onFileDownloadFailed(taskID);
            }
        }
    }

    @Override
    protected void onCancelled() {
        state = TaskState.CANCELLED;
    }

    @Override
    public DownloadTaskInfo getTaskInfo() {
        DownloadTaskInfo info = new DownloadTaskInfo(account, taskID, state, repoID,
                repoName, path, localPath, totalSize, finished, err);
        return info;
    }

    public String getLocalPath() {
        return localPath;
    }
}