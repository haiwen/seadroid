package com.seafile.seadroid2.monitor;

import android.os.Handler;
import android.util.Log;

import com.google.common.collect.ConcurrentHashMultiset;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Update modified files, retry until success
 */
public class AutoUpdateManager implements Runnable, CachedFileChangedListener {
    private static final String DEBUG_TAG = "AutoUpdateManager";
    private static final int CHECK_INTERVAL_MILLI = 3000;

    private TransferService txService;
    private Thread thread;
    private volatile boolean running;
    private final Handler mHandler = new Handler();

    private Set<AutoUpdateInfo> infos = Sets.newHashSet();
    private MonitorDBHelper db = MonitorDBHelper.getInstance();

    public void onTransferServiceConnected(TransferService txService) {
        this.txService = txService;
        running = true;
        thread = new Thread(this);
        thread.setUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
            @Override
            public void uncaughtException(Thread thread, Throwable ex) {
                Log.e(DEBUG_TAG, "Uncaught exception", ex);
            }
        });
        thread.start();
    }

    public void stop() {
        running = false;
    }

    /**
     * This method is called by file monitor, so it would be executed in the file monitor thread
     */
    @Override
    public void onCachedBlocksChanged(final Account account, final SeafCachedFile cachedFile, final File localFile) {
        addTask(account, cachedFile, localFile);
    }

    /**
     * This method is called by file monitor, so it would be executed in the file monitor thread
     */
    @Override
    public void onCachedFileChanged(final Account account, final SeafCachedFile cachedFile, final File localFile) {
        addTask(account, cachedFile, localFile);
    }

    public void addTask(Account account, SeafCachedFile cachedFile, File localFile) {
        AutoUpdateInfo info = new AutoUpdateInfo(account, cachedFile.repoID, cachedFile.repoName,
                Utils.getParentPath(cachedFile.path), localFile.getPath());

        synchronized (infos) {
            if (infos.contains(info)) {
                return;
            }
            infos.add(info);
        }

        db.saveAutoUpdateInfo(info);

        if (!Utils.isNetworkOn() || txService == null) {
            return;
        }

        ArrayList<AutoUpdateInfo> infosList = new ArrayList<AutoUpdateInfo>(1);
        infosList.add(info);
        addAllUploadTasks(infosList);
    }

    private void addAllUploadTasks(final List<AutoUpdateInfo> infos) {
        mHandler.post(new Runnable() {
            @Override
            public void run() {
                for (AutoUpdateInfo info : infos) {
                    if (info.canLocalDecrypt()) {
                        txService.addTaskToUploadQue(info.account, info.repoID, info.repoName,
                                info.parentDir, info.localPath, true, true);
                    } else {
                        txService.addUploadTask(info.account, info.repoID, info.repoName,
                                info.parentDir, info.localPath, true, true);
                    }
                }
            }
        });
    }

    /**
     * This callback in called in the main thread when the transfer service broadcast is received
     */
    public void onFileUpdateSuccess(Account account, String repoID, String repoName,
                                    String parentDir, String localPath, int version) {
        // This file has already been updated on server, so we abort auto update task
        if (removeAutoUpdateInfo(account, repoID, repoName, parentDir, localPath)) {
            Log.d(DEBUG_TAG, "auto updated " + localPath);
        }
    }

    private static int MAX_UPLOAD_FAILURES = 2;
    private ConcurrentHashMultiset<AutoUpdateInfo> uploadFailuresByFile = ConcurrentHashMultiset.create();

    private boolean maxFailureReached(Account account, String repoID, String repoName,
                                      String parentDir, String localPath, int version) {
        AutoUpdateInfo info = new AutoUpdateInfo(account, repoID, repoName, parentDir, localPath);
        int failures = uploadFailuresByFile.count(info) + 1;
        if (failures >= MAX_UPLOAD_FAILURES) {
            uploadFailuresByFile.remove(info);
            return true;
        }
        uploadFailuresByFile.setCount(info, failures);
        return false;
    }

    public void onFileUpdateFailure(Account account,
                                    String repoID,
                                    String repoName,
                                    String parentDir,
                                    String localPath,
                                    SeafException e,
                                    int version) {
        boolean shouldAbortUpload = false;
        if (e.getCode() / 100 == 4) {
            // This file has already been removed on server, so we abort the auto update task
            shouldAbortUpload = true;
        }

        if (!shouldAbortUpload
            && maxFailureReached(account, repoID, repoName, parentDir, localPath, version)) {
            Log.d(DEBUG_TAG,
                String.format("abort auto updating %s because failed for more than %s times",
                    localPath, MAX_UPLOAD_FAILURES));
            shouldAbortUpload = true;
        }

        if (!shouldAbortUpload) {
            return;
        }

        if (removeAutoUpdateInfo(account, repoID, repoName, parentDir, localPath)) {
            Log.d(DEBUG_TAG, String.format("failed to auto update %s, error %s", localPath, e));
        } else {
            Log.d(DEBUG_TAG, String.format("failed to remove auto update task for %s", localPath));
        }
    }

    private boolean removeAutoUpdateInfo(Account account, String repoID, String repoName, String parentDir, String localPath) {
        final AutoUpdateInfo info = new AutoUpdateInfo(account, repoID, repoName, parentDir, localPath);
        boolean exist = false;

        synchronized (infos) {
            exist = infos.remove(info);
        }

        if (exist) {
            ConcurrentAsyncTask.submit(new Runnable() {
                @Override
                public void run() {
                    db.removeAutoUpdateInfo(info);
                }
            });
        }
        return exist;
    }

    /**
     * Periodically checks the upload tasks and schedule them to run
     **/
    private void scheduleUpdateTasks() {
        int size = infos.size();
        if (!Utils.isNetworkOn()) {
            Log.d(DEBUG_TAG, "network is not available, " + size + " in queue");
            return;
        }

        if (txService == null) {
            return;
        }

        Log.v(DEBUG_TAG, String.format("check auto upload tasks, %d in queue", size));

        List<AutoUpdateInfo> infosList;
        synchronized (infos) {
            if (infos.isEmpty()) {
                return;
            }
            infosList = ImmutableList.copyOf(infos);
        }

        addAllUploadTasks(infosList);
    }

    public void run() {
        synchronized (infos) {
            infos.addAll(db.getAutoUploadInfos());
        }

        while (running) {
            scheduleUpdateTasks();
            if (!running) {
                break;
            }
            try {
                Thread.sleep(CHECK_INTERVAL_MILLI);
            } catch (final InterruptedException ignored) {
                break;
            }
        }
    }
}
