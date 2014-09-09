package com.seafile.seadroid2.sync;

import android.util.Log;

import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedPhoto;
import com.seafile.seadroid2.util.Utils;

public class CameraUploadManager {
    
    private static final String DEBUG_TAG = "CameraUploadManager";
    
    private CameraUploadDBHelper dbHelper;
    private Account account;
    
    public CameraUploadManager(Account act) {
        account = act;
        dbHelper = CameraUploadDBHelper.getCameraUploadDBHelper();
    }

    /**
     * 
     * @param repoName
     * @param repoID
     * @param path
     * @return
     */
    public SeafCachedPhoto getCachedPhoto(String repoName, String repoID,
            String path) {
        SeafCachedPhoto cp = dbHelper.getPhotoCacheItem(repoID, path);
        return cp;
    }

    /**
     * 
     * @param repoName
     * @param repoID
     * @param dir
     * @param path
     * @return
     */
    public SeafCachedPhoto getCachedPhoto(String repoName, String repoID,
            String dir, String path) {
        String validPath = Utils.pathJoin(dir, path);
        Log.v(DEBUG_TAG, "validPath:" + validPath);
        return getCachedPhoto(repoName, repoID, validPath);
    }

    /**
     * 
     * @param repoName
     * @param repoID
     * @param path
     */
    public void addCachedPhoto(String repoName, String repoID, String path) {
        Log.d(DEBUG_TAG, "path: " + path);
        SeafCachedPhoto item = new SeafCachedPhoto();
        item.repoName = repoName;
        item.repoID = repoID;
        item.path = path;
        item.accountSignature = account.getSignature();
        dbHelper.savePhotoCacheItem(item);
    }

    public int removeCachedPhoto(SeafCachedPhoto cp) {
        // unused method
        // check if the file deletion succeeds
        return dbHelper.removePhotoCacheItem(cp);
    }
    
    public void onPhotoUploadSuccess(final String repoName, final String repoID, final String path){
        ConcurrentAsyncTask.execute(new Runnable() {
            @Override
            public void run() {
                addCachedPhoto(repoName, repoID, path);
            }
        });
    }
}
