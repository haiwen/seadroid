package com.seafile.seadroid2.sync;

import java.util.List;

import android.util.Pair;

import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.data.SeafCachedPhoto;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.util.Utils;

public class CameraUploadManager {
    private static final String DEBUG_TAG = "CameraUploadManager";

    private CameraUploadDBHelper dbHelper;
    private DatabaseHelper mDatabaseHelper;
    private DataManager mDataManager;
    private SeafConnection sc;
    private Account account;
    
    public CameraUploadManager(Account act) {
        account = act;
        sc = new SeafConnection(act); 
        dbHelper = CameraUploadDBHelper.getCameraUploadDBHelper();
        mDatabaseHelper = DatabaseHelper.getDatabaseHelper();
        mDataManager = new DataManager(act);
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
        return getCachedPhoto(repoName, repoID, validPath);
    }

    /**
     * 
     * @param repoName
     * @param repoID
     * @param path
     */
    public void addCachedPhoto(String repoName, String repoID, String path) {
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
    
    /**
     * 
     * @param repoID
     * @param parentDir
     * @param dirName
     */
    public void createNewDir(final String repoID, final String parentDir, final String dirName) {
        try {
            List<SeafDirent> list = mDataManager.getDirentsFromServer(repoID, parentDir);
            for (SeafDirent seafDirent : list) {
                if (seafDirent.name.equals(CameraUploadService.CAMERA_UPLOAD_REMOTE_DIR)) {
                    return;
                }
            }
        } catch (SeafException e) {
            e.printStackTrace();
        }
            
        Pair<String, String> rlt = null;
        try {
            rlt = sc.createNewDir(repoID, parentDir, dirName);
        } catch (SeafException e) {
            e.printStackTrace();
        }
        String newDirID = rlt.first;
        String response = rlt.second;

        // The response is the dirents of the parentDir after creating
        // the new dir. We save it to avoid request it again
        mDatabaseHelper.saveDirents(repoID, parentDir, newDirID, response);
    }
}
