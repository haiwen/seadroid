package com.seafile.seadroid2.transfer;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ProgressMonitor;

import org.json.JSONException;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

/**
 * Download task
 *
 */
public class DownloadTask extends TransferTask {
    public static final String DEBUG_TAG = "DownloadTask";

    private String localPath;
    private DownloadStateListener downloadStateListener;
    private boolean byBlock;
    private boolean updateTotal;
    private boolean blockRemaining;
    private int encVersion;

    public DownloadTask(int taskID, Account account, String repoName, String repoID, String path, boolean byBlock, int encVersion,
                        DownloadStateListener downloadStateListener) {
        super(taskID, account, repoName, repoID, path);
        this.byBlock = byBlock;
        this.encVersion = encVersion;
        this.downloadStateListener = downloadStateListener;
    }

    /**
     * When downloading a file, we don't know the file size in advance, so
     * we make use of the first progress update to return the file size.
     */
    @Override
    protected void onProgressUpdate(Long... values) {
        if (totalSize == -1 || updateTotal) {
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
            if (byBlock) {
                return dataManager.getFileByBlocks(repoName, repoID, path, encVersion,
                        new ProgressMonitor() {

                            @Override
                            public void onProgressNotify(long total, boolean updateTotal, boolean blockRemaining) {
                                DownloadTask.this.updateTotal = updateTotal;
                                DownloadTask.this.blockRemaining = blockRemaining;
                                publishProgress(total);
                            }

                            @Override
                            public boolean isCancelled() {
                                return DownloadTask.this.isCancelled();
                            }
                        }
                );
            } else
                return dataManager.getFile(repoName, repoID, path,
                        new ProgressMonitor() {

                            @Override
                            public void onProgressNotify(long total, boolean updateTotal, boolean isLastBlock) {
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
        } catch (JSONException e) {
            err = SeafException.unknownException;
            e.printStackTrace();
            return null;
        } catch (IOException e) {
            err = SeafException.networkException;
            e.printStackTrace();
            return null;
        } catch (NoSuchAlgorithmException e) {
            err = SeafException.unknownException;
            e.printStackTrace();
            return null;
        }
    }

    @Override
    protected void onPostExecute(File file) {
        if (downloadStateListener != null) {
            if (file != null) {
                if (byBlock) {
                    state = (blockRemaining ? TaskState.TRANSFERRING : TaskState.FINISHED);
                    downloadStateListener.onFileDownloadProgress(taskID);
                } else {
                    state = TaskState.FINISHED;
                    downloadStateListener.onFileDownloaded(taskID);
                }

                localPath = file.getPath();
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