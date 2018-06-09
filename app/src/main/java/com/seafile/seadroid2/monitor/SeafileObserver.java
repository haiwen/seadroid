package com.seafile.seadroid2.monitor;

import android.util.Log;

import com.google.common.collect.Maps;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.util.Utils;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationObserver;

import java.io.File;
import java.util.List;
import java.util.Map;

public class SeafileObserver implements FileAlterationListener {
    private static final String DEBUG_TAG = "SeafileObserver";

    private Account account;
    private DataManager dataManager;
    private FileAlterationObserver alterationObserver;

    private final Map<String, SeafCachedFile> watchedFiles = Maps.newHashMap();
    private final CachedFileChangedListener listener;
    private final RecentDownloadedFilesWorkAround recentDownloadedFiles =
            new RecentDownloadedFilesWorkAround();

    public SeafileObserver(Account account, CachedFileChangedListener listener) {
        this.account = account;
        this.dataManager = new DataManager(account);
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
            //save file size , when file change , use it comparing with new size
            cached.fileOriginalSize = file.length();
            if (file.exists()) {
                watchedFiles.put(file.getPath(), cached);
            }
        }
        Log.d(DEBUG_TAG, "watching files, # total watched " + watchedFiles.size());
    }

    public void watchDownloadedFile(String repoID, String repoName, String pathInRepo,
            String localpath) {
        recentDownloadedFiles.addRecentDownloadedFile(localpath);

        SeafCachedFile cacheInfo = new SeafCachedFile();
        cacheInfo.repoID = repoID;
        cacheInfo.repoName = repoName;
        cacheInfo.path = pathInRepo;
        watchedFiles.put(localpath, cacheInfo);

        Log.d(DEBUG_TAG, "start watch downloaded file " + pathInRepo + ", # total watched " + watchedFiles.size());
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
        Log.v(DEBUG_TAG, directory.getPath() + " was modified!");
    }

    @Override
    public void onDirectoryCreate(File directory) {
        Log.v(DEBUG_TAG, directory.getPath() + " was created!");
    }

    @Override
    public void onDirectoryDelete(File directory) {
        Log.v(DEBUG_TAG, directory.getPath() + " was deleted!");
    }

    @Override
    public void onFileChange(File file) {
        String path = file.getPath();

        if (recentDownloadedFiles.isRecentDownloadedFiles(path)) {
            Log.d(DEBUG_TAG, "ignore change signal for recent downloaded file " + path);
            return;
        } else {
            recentDownloadedFiles.removeRecentDownloadedFile(path);
        }

        Log.d(DEBUG_TAG, path + " was modified!");
        SeafCachedFile cachedFile = watchedFiles.get(path);
        if (cachedFile != null) {
            //  office or txt file need to update file
            boolean isTextFile = Utils.isTextFile(file);
            //if file is not TextFile and file size not changed, do not update
            if (file.length() == cachedFile.fileOriginalSize && !isTextFile) {
                return;
            }

            cachedFile.fileOriginalSize = file.length();
            final SeafRepo repo = dataManager.getCachedRepoByID(cachedFile.repoID);
            if (repo != null && repo.canLocalDecrypt()) {
                listener.onCachedBlocksChanged(account, cachedFile, file);
            } else {
                listener.onCachedFileChanged(account, cachedFile, file);
            }
        }
    }

    @Override
    public void onFileCreate(File file) {
        Log.v(DEBUG_TAG, file.getPath() + " was created!");
    }

    @Override
    public void onFileDelete(File file) {
        Log.v(DEBUG_TAG, file.getPath() + " was deleted!");
        String path = file.getPath();
        watchedFiles.remove(path);
        recentDownloadedFiles.removeRecentDownloadedFile(path);
        Log.d(DEBUG_TAG, "now watching files, # total watched " + watchedFiles.size());
    }

    @Override
    public void onStart(FileAlterationObserver fao) {
        Log.v(DEBUG_TAG, fao.toString() + " start checking event!");
    }

    @Override
    public void onStop(FileAlterationObserver fao) {
        Log.v(DEBUG_TAG, fao.toString() + " finished checking event!");
    }

    /**
     * When user downloads a file, the outdated file is replaced, so the onFileChange signal would
     * be triggered, which we should not treat it as a modification. This class provides a workaroud
     * for this.
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

        public void removeRecentDownloadedFile(String filePath) {
            recentDownloadedFiles.remove(filePath);
            Log.d(DEBUG_TAG, "remove recent file, # total watched " + recentDownloadedFiles.size());
        }
    }
}
