package com.seafile.seadroid2.monitor;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationObserver;

import android.util.Log;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;

public class SeafileObserver implements FileAlterationListener {
    private final String DEBUG_TAG = "SeafileObserver";
    private Account account;
    private Map<File, SeafCachedFile> fileMap = new HashMap<File, SeafCachedFile>();
    private FileAlterationObserver alterationObserver;

    private CachedFileChangedListener listener;

    public interface CachedFileChangedListener {
        void onCachedFiledChanged(Account account, SeafCachedFile cf, File file);
    }

    public SeafileObserver(Account account, CachedFileChangedListener listener) {
        this.account = account;
        this.listener = listener;
        alterationObserver = new FileAlterationObserver(getAccountDir());
        alterationObserver.addListener(this);
        addCachedFiles();
    }

    private String getAccountDir() {
        DataManager dataManager = new DataManager(account);
        return dataManager.getAccountDir();
    }

    public FileAlterationObserver getAlterationObserver() {
        return alterationObserver;
    }

    private void addCachedFiles() {
        DataManager dataManager = new DataManager(account);
        List<SeafCachedFile> cachedfiles = dataManager.getCachedFiles();
        for (SeafCachedFile cached : cachedfiles) {
            File file = dataManager.getLocalRepoFile(cached.repoName, cached.repoID, cached.path);
            if (file.exists()) {
                fileMap.put(file, cached);
            }
        }
    }

    public void addToMap(SeafCachedFile cachedFile) {
        DataManager dataManager = new DataManager(account);
        File file = dataManager.getLocalRepoFile(cachedFile.repoName, cachedFile.repoID, cachedFile.path);
        fileMap.put(file, cachedFile);
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
        Log.d(DEBUG_TAG, file.getPath() + " was modified!");
        SeafCachedFile cachedFile = fileMap.get(file);
        if (cachedFile != null) {
            listener.onCachedFiledChanged(account, cachedFile, file);
            // if (mTransferService != null) {
            //     mTransferService.addUploadTask(account,cachedFile.repoID, cachedFile.repoName, Utils.getParentPath(cachedFile.path), file.getPath(), true);
            // }
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
}
