package com.seafile.seadroid2.monitor;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationObserver;

import android.util.Log;

import com.google.common.collect.Maps;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;

public class SeafileObserver implements FileAlterationListener {
    private final String DEBUG_TAG = "SeafileObserver";
    private Account account;
    private DataManager dataManager;
    private FileAlterationObserver alterationObserver;

    private final Map<String, SeafCachedFile> watchedFiles = Maps.newHashMap();
    private final CachedFileChangedListener listener;
    private final RecentDownloadedFilesWorkAround recentDownloadedFiles = new RecentDownloadedFilesWorkAround();

    public interface CachedFileChangedListener {
        void onCachedFiledChanged(Account account, SeafCachedFile cf, File file);
    }

    public SeafileObserver(Account account, CachedFileChangedListener listener) {
        this.account = account; this.dataManager = new DataManager(account);
        this.listener = listener;
        alterationObserver = new FileAlterationObserver(getAccountDir());
        alterationObserver.addListener(this);
        watchAllCachedFiles();
    }

    private String getAccountDir() {
        return dataManager.getAccountDir();
    }

    public FileAlterationObserver getAlterationObserver() {
        return alterationObserver;
    }

    private void watchAllCachedFiles() {
        List<SeafCachedFile> cachedfiles = dataManager.getCachedFiles();
        for (SeafCachedFile cached : cachedfiles) {
            File file = dataManager.getLocalRepoFile(cached.repoName, cached.repoID, cached.path);
            if (file.exists()) {
                watchedFiles.put(file.getPath(), cached);
            }
        }
    }

    public void watchDownloadedFile(String repoID, String repoName, String filePathInRepo, String localpath) {
        recentDownloadedFiles.addRecentDownloadedFile(localpath);

        SeafCachedFile cacheInfo = new SeafCachedFile();
        cacheInfo.repoID = repoID;
        cacheInfo.repoName = repoName;
        cacheInfo.path = filePathInRepo;
        watchedFiles.put(localpath, cacheInfo);
    }

    public void setAccount(Account account) {
        this.account = account;
    }

    public Account getAccount() {
        return account;
    }

    public void startWatching() {
        try {
            alterationObserver.initialize();
        } catch (Exception e) {

        }
        alterationObserver.checkAndNotify();
    }

    public void stopWatching() {
        try {
            alterationObserver.destroy();
        } catch (Exception e) {

        }
    }

    @Override
    public void onDirectoryChange(File directory) {
        Log.d(DEBUG_TAG, directory.getPath() + " was modified!");
    }

    @Override
    public void onDirectoryCreate(File directory) {
        Log.d(DEBUG_TAG, directory.getPath() + " was created!");
    }

    @Override
    public void onDirectoryDelete(File directory) {
        Log.d(DEBUG_TAG, directory.getPath() + " was deleted!");
    }

    @Override
    public void onFileChange(File file) {
        String path = file.getPath();

        if (recentDownloadedFiles.isRecentDownloadedFiles(path)) {
            Log.d(DEBUG_TAG, "ignore change signal for recent downloaded file " + path);
            return;
        }

        Log.d(DEBUG_TAG, path + " was modified!");
        SeafCachedFile cachedFile = watchedFiles.get(path);
        if (cachedFile != null) {
            listener.onCachedFiledChanged(account, cachedFile, file);
        }
    }

    @Override
    public void onFileCreate(File file) {
        Log.d(DEBUG_TAG, file.getPath() + " was created!");
    }

    @Override
    public void onFileDelete(File file) {
        Log.d(DEBUG_TAG, file.getPath() + " was deleted!");
    }

    @Override
    public void onStart(FileAlterationObserver fao) {
        Log.d(DEBUG_TAG, fao.toString() + " start checking event!");
    }

    @Override
    public void onStop(FileAlterationObserver fao) {
        Log.d(DEBUG_TAG, fao.toString() + " finished checking event!");
    }

    /**
     * When user downloads a file, the outdated file is replaced, so the
     * onFileChange signal would be triggered, which we should not treat it as
     * a modification. This class provides a workaroud for this.
     */
    private static class RecentDownloadedFilesWorkAround {
        private final Map<String, Long> recentDownloadedFiles = Maps.newConcurrentMap();

        public boolean isRecentDownloadedFiles(String filePath) {
            Long timestamp = recentDownloadedFiles.get(filePath);
            if (timestamp != null) {
                long timeWhenDownloaded = timestamp;
                long now = Utils.now();

                if (now - timeWhenDownloaded < 10000) {
                    return true;
                }
            }

            return false;
        }

        public void addRecentDownloadedFile(String filePath) {
            recentDownloadedFiles.put(filePath, Utils.now());
        }
    }
}
