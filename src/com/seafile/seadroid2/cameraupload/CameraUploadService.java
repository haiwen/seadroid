package com.seafile.seadroid2.cameraupload;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.database.ContentObserver;
import android.database.Cursor;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Binder;
import android.os.IBinder;
import android.provider.MediaStore;
import android.provider.MediaStore.MediaColumns;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedPhoto;
import com.seafile.seadroid2.transfer.*;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.util.CameraUploadUtil;

public class CameraUploadService extends Service {
    private static final String DEBUG_TAG = "CameraUploadService";

    public static final String DIR = "/";
    public static final String CAMERA_UPLOAD_REMOTE_DIR = "Camera Uploads";
    public static final String CAMERA_UPLOAD_REMOTE_PARENTDIR = "/";
    public static final String BROADCAST_CAMERA_UPLOAD_LIBRARY_NOT_FOUND = "cameraUploadLibarayNotFound";
    public static final String BROADCAST_CAMERA_UPLOAD_SERVICE_STARTED = "cameraUploadServiceStarted";
    public static final String BROADCAST_CAMERA_UPLOAD_SERVICE_STOPPED = "cameraUploadServiceStopped";
    private ArrayList<PendingUploadInfo> pendingUploads = Lists.newArrayList();
    private CameraObserver cameraUploadObserver = new CameraObserver();
    private CameraUploadManager cUploadManager;
    private TransferService mTransferService;
    private SettingsManager settingsMgr;
    private final IBinder mBinder = new CameraBinder();
    private boolean isNetworkAvailable;
    private boolean isRemoteCameraUploadRepoValid;
    private boolean isCameraUploadEnabled;
    private Account account;
    private String accountEmail;
    private String accountServer;
    private String accountToken;
    private String repoId;
    private String repoName;

    @Override
    public void onCreate() {
        Log.d(DEBUG_TAG, "onCreate");
        
        settingsMgr = SettingsManager.instance();
        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");

        this.getApplicationContext()
        .getContentResolver()
        .registerContentObserver(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI, false,
                cameraUploadObserver);
        LocalBroadcastManager.getInstance(this).registerReceiver(transferReceiver,
                new IntentFilter(TransferManager.BROADCAST_ACTION));
    }

    private void cancelUploadTasks(){

        mTransferService.cancelAllCameraUploadTasks();
        Intent localIntent = new Intent(TransferManager.BROADCAST_ACTION).putExtra("type",
                BROADCAST_CAMERA_UPLOAD_SERVICE_STOPPED);
        LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(localIntent);
    }

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        cancelUploadTasks();
        this.getApplicationContext().getContentResolver()
        .unregisterContentObserver(cameraUploadObserver);
        cameraUploadObserver = null;
        if (mTransferService != null) {
            unbindService(mConnection);
            mTransferService = null;
        }

        LocalBroadcastManager.getInstance(this).unregisterReceiver(transferReceiver);
        transferReceiver = null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");

        initParams();
        if (repoId != null && accountEmail != null) {
            isCameraUploadEnabled = true;
            account = new Account(accountServer, accountEmail, accountToken);
            cUploadManager = new CameraUploadManager(account);
        } else
            isCameraUploadEnabled = false;

        isNetworkAvailable = settingsMgr.checkCameraUploadNetworkAvailable();
        // ensure network is available
        if (!isNetworkAvailable) {
            // do nothing until network connection available
            return START_STICKY;
        }

        if (isCameraUploadEnabled) {
            ConcurrentAsyncTask.execute(new PhotoUploadTask());
        }

        return START_STICKY;
    }

    private void initParams() {
        repoId = settingsMgr.getCameraUploadRepoId();
        repoName = settingsMgr.getCameraUploadRepoName();
        accountEmail = settingsMgr.getCameraUploadAccountEmail();
        accountServer = settingsMgr.getCameraUploadAccountServer();
        accountToken = settingsMgr.getCameraUploadAccountToken();
    }

    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferBinder binder = (TransferBinder) service;
            mTransferService = binder.getService();

            for (PendingUploadInfo info : pendingUploads) {
               mTransferService.addTaskToUploadQue(account, info.repoID,
                                        info.repoName, info.targetDir,
                                        info.localFilePath, info.isUpdate, info.isCopyToLocal);
            }
            pendingUploads.clear();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            mTransferService = null;
        }
    };

    private void addCameraUploadTask(String repoID, String repoName, String targetDir, String localFilePath) {
        if (mTransferService != null) {
            // set the last parameter "isUpdate" to true to stop copying file into sd-card
            mTransferService.addTaskToUploadQue(account, repoID, repoName, targetDir, localFilePath, false, false);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false, false);
            pendingUploads.add(info);
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        Log.d(DEBUG_TAG, "onBind");
        return mBinder;
    }

    public class CameraBinder extends Binder {
        public CameraUploadService getService() {
            return CameraUploadService.this;
        }
    }

    private class CameraObserver extends ContentObserver {
        public CameraObserver() {
            super(null);
        }

        @Override
        public void onChange(boolean selfChange) {
            super.onChange(selfChange);
            isNetworkAvailable = settingsMgr.checkCameraUploadNetworkAvailable();
            if (!isNetworkAvailable) {
                // do nothing until network connection available
                return;
            }
            if (!isRemoteCameraUploadRepoValid) {
                return;
            }

            ConcurrentAsyncTask.execute(new CameraEventReceiverTask());
        }
    }

    private File getPhotoFromMediaStore(Context context, Uri uri) {
        Cursor cursor = context.getContentResolver().query(uri, null, null,
                null, "date_added DESC");
        File photo = null;
        if (cursor.moveToNext()) {
            int dataColumn = cursor.getColumnIndexOrThrow(MediaColumns.DATA);
            String filePath = cursor.getString(dataColumn);
            photo = new File(filePath);
        }
        cursor.close();
        return photo;
    }

    private class PhotoUploadTask extends AsyncTask<Void, Void, Void> {
        @Override
        protected Void doInBackground(Void... params) {

            // ensure remote camera upload library exists
            try {
                isRemoteCameraUploadRepoValid = cUploadManager
                        .isRemoteCameraUploadRepoValid(repoId, CAMERA_UPLOAD_REMOTE_PARENTDIR);
                if (!isRemoteCameraUploadRepoValid) {
                    return null;
                }
                // create a remote "Camera Uploads" folder if deleted
                cUploadManager.validateRemoteCameraUploadsDir(
                        repoId,
                        CAMERA_UPLOAD_REMOTE_PARENTDIR,
                        CAMERA_UPLOAD_REMOTE_DIR);
            } catch (SeafException e) {
                e.printStackTrace();
            }

            boolean isVideosAllowed = settingsMgr.isVideosUploadAllowed();
            List<File> pathList;

            if (settingsMgr.isCustomScanDir()) {
                pathList = CameraUploadUtil.getCustomPathList(isVideosAllowed);
            } else
                pathList = CameraUploadUtil.getAutoScannedPathList(isVideosAllowed);

            if (pathList != null) {
                for (File photo : pathList) {
                    String path = photo.getName();
                    // use local database to detect duplicate upload
                    SeafCachedPhoto cp = cUploadManager.getCachedPhoto(repoName, repoId, DIR, path);
                    if (cp == null) {
                        // add photos to uploading queue
                        addCameraUploadTask(repoId, repoName, CAMERA_UPLOAD_REMOTE_PARENTDIR + CAMERA_UPLOAD_REMOTE_DIR, photo.getAbsolutePath());
                    }
                }

            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (!isRemoteCameraUploadRepoValid) {
                Intent localIntent = new Intent(TransferManager.BROADCAST_ACTION).putExtra("type",
                        BROADCAST_CAMERA_UPLOAD_LIBRARY_NOT_FOUND);
                LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(localIntent);
            }

        }
    }

    private class CameraEventReceiverTask extends AsyncTask<Void, Void, File> {
        // private String detectLog;
        @Override
        protected File doInBackground(Void... params) {
            return getPhotoFromMediaStore(getApplicationContext(),
                    MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
        }

        @Override
        protected void onPostExecute(File photo) {
            if (photo == null)
                return;

            String detectLog = "detected " + photo.getName();

            boolean isVideosAllowed = settingsMgr.isVideosUploadAllowed();
            boolean isCustomScan = settingsMgr.isCustomScanDir();

            // only upload files under some specific folders
            boolean isPathValid = CameraUploadUtil.isPathValid(photo, isVideosAllowed, isCustomScan);
            Log.d(DEBUG_TAG, "path "
                    + photo.getAbsolutePath()
                    + " isVideoAllowed "
                    + isVideosAllowed
                    + " isCustomScan "
                    + isCustomScan
                    + " isPathValid "
                    + isPathValid);

            if (!isPathValid)
                return;

            boolean isFileTypeValid = false;
            // only upload photos or videos if allowed to
            if ((CameraUploadUtil.isVideoType(photo.getName()) && isVideosAllowed)
                    || CameraUploadUtil.isImageType(photo.getName())) {
                isFileTypeValid = true;
            }

            Log.d(DEBUG_TAG, detectLog);

            if (repoName == null || repoId == null)
                return;

            SeafCachedPhoto cachePhoto = cUploadManager.getCachedPhoto(repoName, repoId, DIR,
                    photo.getName());
            if (cachePhoto == null
                    && isFileTypeValid) {
                addCameraUploadTask(repoId, repoName, CAMERA_UPLOAD_REMOTE_PARENTDIR
                        + CAMERA_UPLOAD_REMOTE_DIR, photo.getAbsolutePath());
            }
        }
    }

    private BroadcastReceiver transferReceiver = new BroadcastReceiver() {

        @Override
        public void onReceive(Context context, Intent intent) {
            if (mTransferService == null) {
                return;
            }

            String type = intent.getStringExtra("type");
            if (type == null) {
                return;
            }
            List<String> list = Lists.newArrayList();

            if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                UploadTaskInfo info = mTransferService.getUploadTaskInfo(taskID);

                if (info != null) {
                    if (!list.contains(info.localFilePath)) {
                        cUploadManager.onPhotoUploadSuccess(info.repoName,
                                info.repoID, info.localFilePath
                                        .substring(info.localFilePath
                                                .lastIndexOf(DIR)));
                        list.add(info.localFilePath);
                    }
                }
            }
        }
    };
}
