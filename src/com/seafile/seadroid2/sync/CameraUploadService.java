package com.seafile.seadroid2.sync;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.database.ContentObserver;
import android.database.Cursor;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Binder;
import android.os.IBinder;
import android.provider.MediaStore;
import android.provider.MediaStore.MediaColumns;
import android.support.v4.app.NotificationCompat;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

import com.seafile.seadroid2.AccountsActivity;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.SeafCachedPhoto;
import com.seafile.seadroid2.fileschooser.SelectableFile;
import com.seafile.seadroid2.transfer.PendingUploadInfo;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.SettingsFragment;
import com.seafile.seadroid2.util.Utils;

public class CameraUploadService extends Service {
    private static final String DEBUG_TAG = "CameraUploadService";
    
    public static final int NOTIFICATION_ID = 1;
    private final IBinder mBinder = new CameraBinder();
    private CameraObserver cameraUploadObserver = new CameraObserver();
    private ArrayList<PendingUploadInfo> pendingUploads = new ArrayList<PendingUploadInfo>();
    private NotificationManager mNotificationManager;
    private NotificationCompat.Builder builder;
    private String repoId;
    private String repoName;
    private String accountEmail;
    private String accountServer;
    private String accountToken;
    private Account account;
    private Boolean isCameraUpload = false;
    private CameraUploadManager cUploadManager;
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
        
        LocalBroadcastManager.getInstance(this).registerReceiver(transferReceiver,
                new IntentFilter(TransferService.BROADCAST_ACTION));
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
        taskIds.clear();
        if (mTransferService != null) {
            unbindService(mConnection);
            mTransferService = null;
        }
        this.getApplicationContext().getContentResolver()
                .unregisterContentObserver(cameraUploadObserver);
        cameraUploadObserver = null;
        LocalBroadcastManager.getInstance(this).unregisterReceiver(transferReceiver);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");

        getPreference();
        if (repoId != null && repoName != null && accountEmail != null
                && accountServer != null && accountToken != null) {
            isCameraUpload = true;
            account = new Account(accountServer, accountEmail, null, accountToken);
            cUploadManager = new CameraUploadManager(account);
        }

        if (isCameraUpload) {
            ConcurrentAsyncTask.execute(new PhotoTask());
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

            for (PendingUploadInfo info : pendingUploads) {
               mTransferService.addUploadTask(account, info.repoID,
                                        info.repoName, info.targetDir,
                                        info.localFilePath, info.isUpdate);
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
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false);
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
            ConcurrentAsyncTask.execute(new MediaTask());
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
    private class PhotoTask extends AsyncTask<Void, Void, List<SelectableFile>> {

        @Override
        protected List<SelectableFile> doInBackground(Void... params) {
            return Utils.getPhotoList();
        }

        @Override
        protected void onPostExecute(List<SelectableFile> result) {
            list = result;
            if (list != null) {
                int photosCount = 0;
                for (SelectableFile selectableFile : list) {
                    String path = new File(selectableFile.getAbsolutePath()).getName();
                    SeafCachedPhoto cp = cUploadManager.getCachedPhoto(repoName, repoId, "/", path);
                    if (cp == null) {
                        photosCount++;
                        addUploadTask(repoId, repoName, "/", selectableFile.getAbsolutePath());
                    }
                }
                notifyUser(photosCount, repoName);
            }
        }
    }
    
    private class MediaTask extends AsyncTask<Void, Void, Media> {
        private String detectLog;

        @Override
        protected Media doInBackground(Void... params) {
            return readFromMediaStore(getApplicationContext(), MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
        }

        @Override
        protected void onPostExecute(Media media) {
            detectLog = "detected " + media.file.getName();
            if (repoId != null && accountEmail != null
                    && account.getEmail().equals(accountEmail)
                    && account.getServer().equals(accountServer)) {
                Log.d(DEBUG_TAG, detectLog);
                if (cUploadManager.getCachedPhoto(repoName, repoId, "/", media.file.getName()) == null) {
                    addUploadTask(repoId, repoName, "/", media.file.getAbsolutePath());
                    notifyUser(repoName);
                }
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

            if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                UploadTaskInfo info = mTransferService.getUploadTaskInfo(taskID);

                if (info != null) {
                    cUploadManager.onPhotoUploadSuccess(info.repoName, info.repoID, info.localFilePath);
                }
            }

        }

    };
}