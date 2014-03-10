package com.seafile.seadroid2.monitor;

import java.io.File;
import java.util.List;
import java.util.Set;

import android.os.Handler;
import android.util.Log;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.transfer.TransferService;

/**
 * Update modified files, retry until success
 *
 */
public class AutoUpdateManager implements Runnable {

    private static final String DEBUG_TAG = "AutoUpdateManager";

    private TransferService txService;
    private Thread thread;
    private boolean running;
    private static final int CHECK_INTERVAL_MILLI = 3000;
    private final Handler mHandler = new Handler();

    private Set<AutoUpdateInfo> infos = Sets.newHashSet();

    private MonitorDBHelper db = MonitorDBHelper.getMonitorDBHelper();

    public void onTransferServiceConnected(TransferService txService) {
        this.txService = txService;
        running = true;
        thread = new Thread(this);
        thread.start();
    }

    public void stop() {
        running = false;
    }

    /**
     * This method is called by file monitor, so it would be executed in the file monitor thread
     */
    public void addTask(Account account, SeafCachedFile cachedFile, File localFile) {

        AutoUpdateInfo info =
                new AutoUpdateInfo(account, cachedFile.repoID, cachedFile.repoName,
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

        addUpdateTask(info);
    }

    private void addUpdateTask(final AutoUpdateInfo info) {
        mHandler.post(new Runnable() {
            @Override
            public void run() {
                txService.addUploadTask(info.account, info.repoID, info.repoName, info.parentDir,
                        info.localPath, true);
            }
        });
    }

    private void addAllUploadTasks(final List<AutoUpdateInfo> infos) {
        mHandler.post(new Runnable() {
            @Override
            public void run() {
                for (AutoUpdateInfo info : infos) {
                    txService.addUploadTask(info.account, info.repoID, info.repoName, info.parentDir,
                        info.localPath, true);
                }
            }
        });
    }

    /**
     * This callback in called in the main thread when the transfer service broadcast is received
     */
    public void onFileUpdateSuccess(Account account, String repoID, String repoName,
            String parentDir, String localPath) {
        final AutoUpdateInfo info =
                new AutoUpdateInfo(account, repoID, repoName, parentDir, localPath);
        boolean exist = false;

        synchronized (infos) {
            exist = infos.remove(info);
        }

        if (exist) {
            Log.d(DEBUG_TAG, "auto updated " + localPath);
            ConcurrentAsyncTask.execute(new Runnable() {
                @Override
                public void run() {
                    db.removeAutoUpdateInfo(info);
                }
            });
        }
    }

    public void onFileUpdateFailure(Account account, String repoID, String repoName,
            String parentDir, String localPath, SeafException e) {

        if (e.getCode() / 100 != 4) {
            return;
        }

        // This file has already been removed on server, so we abort the auto update task.
        final AutoUpdateInfo info =
                new AutoUpdateInfo(account, repoID, repoName, parentDir, localPath);

        boolean exist = false;
        synchronized (infos) {
            exist = infos.remove(info);
        }

        if (exist) {
            Log.d(DEBUG_TAG, String.format("failed to auto update %s, error %s", localPath, e));
            ConcurrentAsyncTask.execute(new Runnable() {
                @Override
                public void run() {
                    db.removeAutoUpdateInfo(info);
                }
            });
        }
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

        Log.d(DEBUG_TAG, String.format("check auto upload tasks, %d in queue", size));

        List<AutoUpdateInfo> infosList;
        synchronized (infos) {
            if (infos.size() == 0) {
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


class AutoUpdateInfo {
    final Account account;
    final String repoID;
    final String repoName;
    final String parentDir;
    final String localPath;

    public AutoUpdateInfo(Account account, String repoID, String repoName, String parentDir,
            String localPath) {

        this.account = account;
        this.repoID = repoID;
        this.repoName = repoName;
        this.parentDir = parentDir;
        this.localPath = localPath;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;

        AutoUpdateInfo a = (AutoUpdateInfo) obj;

        return this.account == a.account && this.repoID == a.repoID && this.repoName == a.repoName
                && this.parentDir == a.parentDir && this.localPath == a.localPath;
    }

    private volatile int hashCode = 0;

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = Objects.hashCode(account, repoID, repoName, parentDir, localPath);
        }

        return hashCode;
    }
}
