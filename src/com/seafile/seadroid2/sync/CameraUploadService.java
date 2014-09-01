package com.seafile.seadroid2.sync;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.Service;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.database.ContentObserver;
import android.database.Cursor;
import android.net.Uri;
import android.os.Binder;
import android.os.IBinder;
import android.provider.MediaStore;
import android.provider.MediaStore.MediaColumns;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

import com.seafile.seadroid2.AccountsActivity;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.fileschooser.SelectableFile;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.ui.SettingsFragment;
import com.seafile.seadroid2.util.Utils;

public class CameraUploadService extends Service {
    
    private static final String DEBUG_TAG = "CameraUploadService";
    public static final int NOTIFICATION_ID = 1;
    private final IBinder mBinder = new CameraBinder();
    private CameraObserver cameraUploadObserver = new CameraObserver();
    private ArrayList<BrowserActivity.PendingUploadInfo> pendingUploads = new ArrayList<BrowserActivity.PendingUploadInfo>();
    private NotificationManager mNotificationManager;
    private NotificationCompat.Builder builder;
    private String repoId;
    private String repoName;
    private String accountEmail;
    private String accountServer;
    private String accountToken;
    private Account account;
    private Boolean isCameraUpload = false;
    private DataManager mDataManager;
    private TransferService mTransferService;
    private List<SelectableFile> list;
    private List<Integer> taskIds;
    
    @Override
    public void onCreate() {
        Log.d(DEBUG_TAG, "onCreate");
        taskIds = new ArrayList<Integer>();
        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");

        this.getApplicationContext()
                .getContentResolver()
                .registerContentObserver(
                        MediaStore.Images.Media.EXTERNAL_CONTENT_URI, false,
                        cameraUploadObserver);
    }

    private void cancelUploadTasks(List<Integer> taskIds){
        for (Integer taskId : taskIds) {
            mTransferService.cancelUploadTask(taskId);
        }
    }
    
    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        cancelUploadTasks(taskIds);
        if (mTransferService != null) {
            unbindService(mConnection);
            mTransferService = null;
        }
        this.getApplicationContext().getContentResolver()
                .unregisterContentObserver(cameraUploadObserver);
        cameraUploadObserver = null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");

        getPreference();
        if (repoId != null && repoName != null && accountEmail != null
                && accountServer != null && accountToken != null) {
            isCameraUpload = true;
            account = new Account(accountServer, accountEmail, null,
                    accountToken);
            mDataManager = new DataManager(account);
        }

        if (isCameraUpload) {
            list = Utils.getPhotoList();
        }
        if (list != null) {
            int photosCount = 0;
            for (SelectableFile selectableFile : list) {
                String path = "/"
                        + new File(selectableFile.getAbsolutePath()).getName();
                SeafCachedFile cf = mDataManager.getCachedFile(repoName, repoId, path); 
                if (cf == null || cf.fileID == null ) {
                    photosCount++;
                    addUploadTask(repoId, repoName, "/", selectableFile.getAbsolutePath());
                }
            }
            notifyUser(photosCount, repoName);
        }
        return START_STICKY;
    }
    
    private void getPreference() {
        SharedPreferences sharedPref = getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        repoId = sharedPref.getString(SettingsFragment.SHARED_PREF_CAMERA_UPLOAD_REPO_ID, null);
        repoName = sharedPref.getString(SettingsFragment.SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, null);
        accountEmail = sharedPref.getString(SettingsFragment.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);
        accountServer = sharedPref.getString(SettingsFragment.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        accountToken = sharedPref.getString(SettingsFragment.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, null);
    }
    
    private void notifyUser(String repoName) {
        notifyUser(1, repoName);
    }
    
    private void notifyUser(int count, String repoName) {
        if (count == 0) {
            return;
        }
        // Send Notification
        mNotificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
        // Constructs the Builder object
        builder = new NotificationCompat.Builder(this)
                .setSmallIcon(R.drawable.icon)
                .setContentTitle(getString(R.string.camera_upload_info_title))
                .setContentText(getString(R.string.camera_upload_info, count) + repoName)
                .setDefaults(Notification.DEFAULT_ALL); 
        
        // Including the notification ID allows you to update the notification later on.
        mNotificationManager.notify(NOTIFICATION_ID, builder.build());
    }
    
    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferBinder binder = (TransferBinder) service;
            mTransferService = binder.getService();
            Log.d(DEBUG_TAG, "bind TransferService");

            for (BrowserActivity.PendingUploadInfo info : pendingUploads) {
               int padd_task_id = mTransferService.addUploadTask(account, info.repoID,
                                        info.repoName, info.targetDir,
                                        info.localFilePath, info.isUpdate);
               taskIds.add(padd_task_id);
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
            int task_id = mTransferService.addUploadTask(account, repoID, repoName, targetDir, localFilePath, false);
            taskIds.add(task_id);
        } else {
            BrowserActivity ba = new BrowserActivity();
            BrowserActivity.PendingUploadInfo info = ba.new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false);
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
        private String saved;
        public CameraObserver() {
            super(null);
        }

        @Override
        public void onChange(boolean selfChange) {
            super.onChange(selfChange);
            Media media = readFromMediaStore(getApplicationContext(),
                    MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
            saved = "detected " + media.file.getName();
            if (repoId != null && repoName != null && accountEmail != null
                    && accountServer != null
                    && account.getEmail().equals(accountEmail)
                    && account.getServer().equals(accountServer)) {
                Log.d(DEBUG_TAG, saved);
                if (mDataManager.getCachedFile(repoName, repoId, media.file.getName()) == null) {
                    addUploadTask(repoId, repoName, "/", media.file.getAbsolutePath());
                    notifyUser(repoName);
                }
            }
        }
    }

    private Media readFromMediaStore(Context context, Uri uri) {
        Cursor cursor = context.getContentResolver().query(uri, null, null,
                null, "date_added DESC");
        Media media = null;
        if (cursor.moveToNext()) {
            int dataColumn = cursor.getColumnIndexOrThrow(MediaColumns.DATA);
            String filePath = cursor.getString(dataColumn);
            int mimeTypeColumn = cursor
                    .getColumnIndexOrThrow(MediaColumns.MIME_TYPE);
            String mimeType = cursor.getString(mimeTypeColumn);
            media = new Media(new File(filePath), mimeType);
        }
        cursor.close();
        return media;
    }

    private class Media {
        private File file;
        // private String type;

        public Media(File file, String type) {
            this.file = file;
            // this.type = type;
        }
    }
    
}