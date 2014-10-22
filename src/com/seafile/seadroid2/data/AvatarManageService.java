package com.seafile.seadroid2.data;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
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

import com.seafile.seadroid2.AvatarLoadTask;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.transfer.PendingUploadInfo;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.util.CameraUploadUtil;
import com.seafile.seadroid2.util.Utils;

/**
 *  manage loading, caching, updating Avatar operations from background   
 *
 */
public class AvatarManageService extends Service {
    private static final String DEBUG_TAG = "AvatarManageService";

    private ArrayList<PendingUploadInfo> pendingUploads = new ArrayList<PendingUploadInfo>();
    private SettingsManager settingsManager;
    private AvatarManager avatarManager;
    /*private final IBinder mBinder = new CameraBinder();
    private static int intSendBroadcastOnlyOnceFlag = 0;
    private boolean isNetworkAvailable;
    private boolean isRemoteCameraUploadRepoValid;
    private boolean isCameraUpload;
    private Account account;
    private String accountEmail;
    private String accountServer;
    private String accountToken;
    private String repoId;
    private String repoName;*/
    private ArrayList<Account> accounts;
    private AccountManager accountManager;
    private static Map<String, Avatar> avatars;
    
    @Override
    public void onCreate() {
        Log.d(DEBUG_TAG, "onCreate");
        
        settingsManager = SettingsManager.instance();
        accountManager = new AccountManager(getApplicationContext());
        if (avatars == null) {
            avatars = new HashMap<String, Avatar>();
        } 
        // bind transfer service
        /*Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");*/

        /*this.getApplicationContext()
        .getContentResolver()
        .registerContentObserver(
                MediaStore.Images.Media.EXTERNAL_CONTENT_URI, false,
                cameraUploadObserver);
        LocalBroadcastManager.getInstance(this).registerReceiver(transferReceiver,
                new IntentFilter(TransferService.BROADCAST_ACTION));*/
    }

    /*private void cancelUploadTasks(){
        List<UploadTaskInfo> cameraUploadsTasksList =  mTransferService.getAllUploadTaskInfos();
        for (UploadTaskInfo uploadTaskInfo : cameraUploadsTasksList) {
            mTransferService.cancelUploadTask(uploadTaskInfo.taskID);
        }
        Intent localIntent = new Intent(TransferService.BROADCAST_ACTION).putExtra("type",
                BROADCAST_CAMERA_UPLOAD_SERVICE_STOPPED);
        LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(localIntent);
    }*/

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        /*cancelUploadTasks();
        this.getApplicationContext().getContentResolver()
        .unregisterContentObserver(cameraUploadObserver);
        cameraUploadObserver = null;
        if (mTransferService != null) {
            unbindService(mConnection);
            mTransferService = null;
        }*/

        /*LocalBroadcastManager.getInstance(this).unregisterReceiver(transferReceiver);
        transferReceiver = null;
        intSendBroadcastOnlyOnceFlag = 0;*/
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");

        /*initParams();
        if (repoId != null && accountEmail != null) {
            isCameraUpload = true;
            account = new Account(accountServer, accountEmail, null, accountToken);
            avatarManager = new AvatarManager(account);
        }

        */
        accounts = (ArrayList<Account>) accountManager.getAccountList();
        
        for (Account account : accounts) {
            ConcurrentAsyncTask.execute(new AvatarLoadTask(account));
        }
        return START_STICKY;
    }
    
    private class AvatarLoadTask extends AsyncTask<Void, Void, Avatar> {
        AvatarManager avatarManager;
        Account account;
        public AvatarLoadTask(Account account) {
            this.account = account;
            avatarManager = new AvatarManager(account);
        }
        
        @Override
        protected Avatar doInBackground(Void... params) {
            try {
                Avatar avatar = avatarManager.getAvatar(48); 
                Log.v(DEBUG_TAG, "icon url: " + avatar.getUrl());
                Log.v(DEBUG_TAG, "email : " + account.getEmail());
                
                return avatar;
            } catch (SeafException e) {
                e.printStackTrace();
                return null;
            }
        }

        @Override
        protected void onPostExecute(Avatar avatar) {
            if (avatar == null) {
                return;
            }
            avatars.put(account.getSignature(), avatar);
        }
    }
    
    private void initParams() {
        repoId = settingsManager.getCameraUploadRepoId();
        repoName = settingsManager.getCameraUploadRepoName();
        accountEmail = settingsManager.getCameraUploadAccountEmail();
        accountServer = settingsManager.getCameraUploadAccountServer();
        accountToken = settingsManager.getCameraUploadAccountToken();
    }

    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferBinder binder = (TransferBinder) service;
            mTransferService = binder.getService();

            for (PendingUploadInfo info : pendingUploads) {
               mTransferService.addUploadTask(account, info.repoID,
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

    private void addUploadTask(String repoID, String repoName, String targetDir, String localFilePath) {
        if (mTransferService != null) {
            // set the last parameter "isUpdate" to true to stop copying file into sd-card
            // if passed "false" will cause OOM when uploading photos
            mTransferService.addUploadTask(account, repoID, repoName, targetDir, localFilePath, false, false);
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
        public AvatarManageService getService() {
            return AvatarManageService.this;
        }
    }

    private class CameraObserver extends ContentObserver {
        public CameraObserver() {
            super(null);
        }

        @Override
        public void onChange(boolean selfChange) {
            super.onChange(selfChange);
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

    private boolean checkNetworkStatus() {
        if (!Utils.isNetworkOn()) {
            return false;
        }
        // user does not allow mobile connections
        if (!Utils.isWiFiOn() && !settingsManager.isAllowMobileConnections()) {
            return false;
        }
        // Wi-Fi or 2G/3G/4G connections available
        return true;
    }
    
    private class PhotoUploadTask extends AsyncTask<Void, Void, List<File>> {
        @Override
        protected List<File> doInBackground(Void... params) {
            isNetworkAvailable = checkNetworkStatus();
            // ensure network is available
            if (!isNetworkAvailable) {
                return null;
            }
            
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
            
            return CameraUploadUtil.getAllPhotosAbsolutePathList();
        }

        @Override
        protected void onPostExecute(List<File> result) {
            Intent localIntent;
            if (result == null) {
                if (!isNetworkAvailable) {
                    // do nothing until network connection available
                }
                if (isNetworkAvailable && !isRemoteCameraUploadRepoValid) {
                    localIntent = new Intent(TransferService.BROADCAST_ACTION).putExtra("type",
                            BROADCAST_CAMERA_UPLOAD_LIBRARY_NOT_FOUND);
                    LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(localIntent);
                }
                return;
            }
            if (intSendBroadcastOnlyOnceFlag == 0) {

                localIntent = new Intent(TransferService.BROADCAST_ACTION).putExtra("type",
                        BROADCAST_CAMERA_UPLOAD_SERVICE_STARTED);
                LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(localIntent);
                intSendBroadcastOnlyOnceFlag ++;
            }

            for (File photo : result) {
                String path = photo.getName();
                // use local database to detect duplicate upload
                SeafCachedPhoto cp = cUploadManager.getCachedPhoto(repoName, repoId, DIR, path);
                if (cp == null) {
                    // add photos to uploading queue
                    addUploadTask(repoId, repoName, CAMERA_UPLOAD_REMOTE_PARENTDIR + CAMERA_UPLOAD_REMOTE_DIR, photo.getAbsolutePath());
                }
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
            String detectLog = "detected " + photo.getName();
            Log.d(DEBUG_TAG, detectLog);
            SeafCachedPhoto cachePhoto = cUploadManager.getCachedPhoto(repoName, repoId, DIR,
                    photo.getName());
            if (cachePhoto == null) {
                addUploadTask(repoId, repoName, CAMERA_UPLOAD_REMOTE_PARENTDIR
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
            List<String> list = new ArrayList<String>();

            if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_SUCCESS)) {
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
