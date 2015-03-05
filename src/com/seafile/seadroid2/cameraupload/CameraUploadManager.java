package com.seafile.seadroid2.cameraupload;

import java.util.List;

import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedPhoto;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.util.Utils;

/**
 * send request to server to ensure that Camera Uploads folder is valid
 *
 */
public class CameraUploadManager {
    // private static final String DEBUG_TAG = "CameraUploadManager";

    private CameraUploadDBHelper dbHelper;
    private DataManager mDataManager;
    private Account account;

    public CameraUploadManager(Account act) {
        account = act;
        dbHelper = CameraUploadDBHelper.getCameraUploadDBHelper();
        mDataManager = new DataManager(act);
    }

    /**
     * get cached photo from local database
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
     * get cached photo from local database
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
     * add a photo cache to local database
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

    /**
     * remove a photo cache from local database
     *
     * @param cp
     * @return 1 when successfully removed a cache, otherwise 0
     *
     */
    public int removeCachedPhoto(SeafCachedPhoto cp) {
        // unused method
        // check if the file deletion succeeds
        return dbHelper.removePhotoCacheItem(cp);
    }

    public void onPhotoUploadSuccess(final String repoName, final String repoID, final String path) {
        ConcurrentAsyncTask.execute(new Runnable() {
            @Override
            public void run() {
                addCachedPhoto(repoName, repoID, path);
            }
        });
    }

    /**
     * send request to server to check if one particular folder {@link CameraUploadService#CAMERA_UPLOAD_REMOTE_DIR} exist
     *
     *
     * this method should not be placed in UI thread
     *
     * @param repoID
     * @param parentDir
     * @return
     * @throws SeafException
     */
    public Boolean isRemoteCameraUploadRepoValid(String repoID, String parentDir) throws SeafException {
        List<SeafRepo> list = mDataManager.getReposFromServer();
        if (list != null) {
            for (SeafRepo seafRepo : list) {
                if (seafRepo.id.equals(repoID)) {
                    return true;
                }
            }
        }

        return  false;
    }

    /**
     *
     * camera photos only uploaded to the specific folder called {@link CameraUploadService#CAMERA_UPLOAD_REMOTE_DIR},
     * the folder was placed under the root directory of the selected library
     *
     * get dirents list from server,
     * traverse the dirents list to check if the remote folder {@link CameraUploadService#CAMERA_UPLOAD_REMOTE_DIR} already existed or not
     * if not, create a new one
     *
     *
     * @param repoID
     * @param parentDir
     * @param dirName
     * @throws SeafException
     */
    public void validateRemoteCameraUploadsDir(String repoID, String parentDir, String dirName) throws SeafException {
        List<SeafDirent> list = mDataManager.getDirentsFromServer(repoID, parentDir);

        for (SeafDirent seafDirent : list) {
            if (seafDirent.name.equals(CameraUploadService.CAMERA_UPLOAD_REMOTE_DIR)) {
                return;
            }
        }

       mDataManager.createNewDir(repoID, parentDir, dirName);
    }
}
