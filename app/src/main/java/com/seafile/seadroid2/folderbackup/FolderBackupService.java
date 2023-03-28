package com.seafile.seadroid2.folderbackup;

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Binder;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.text.TextUtils;
import android.util.Log;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.folderbackup.selectfolder.FileBean;
import com.seafile.seadroid2.folderbackup.selectfolder.FileTools;
import com.seafile.seadroid2.folderbackup.selectfolder.StringTools;
import com.seafile.seadroid2.transfer.TransferManager;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.UploadTaskManager;
import com.seafile.seadroid2.util.CameraSyncStatus;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationMonitor;
import org.apache.commons.io.monitor.FileAlterationObserver;
import org.greenrobot.eventbus.EventBus;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class FolderBackupService extends Service {
    private static final String DEBUG_TAG = "FolderBackupService";
    private Map<String, FolderBackupInfo> fileUploaded = new HashMap<>();
    private final IBinder mBinder = new FileBackupBinder();
    private TransferService txService = null;
    private DataManager dataManager;
    private FolderBackupDBHelper databaseHelper;
    private AccountManager accountManager;
    private Account currentAccount;
    private RepoConfig repoConfig;
    private List<String> backupPathsList;
    private FolderBackupReceiver mFolderBackupReceiver;
    private FileAlterationMonitor fileMonitor;

    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }

    public class FileBackupBinder extends Binder {
        public FolderBackupService getService() {
            return FolderBackupService.this;
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    @Override
    public void onCreate() {
        super.onCreate();

        databaseHelper = FolderBackupDBHelper.getDatabaseHelper();
        Intent bIntent = new Intent(this, TransferService.class);

        accountManager = new AccountManager(this);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);

        if (mFolderBackupReceiver == null) {
            mFolderBackupReceiver = new FolderBackupReceiver();
        }

        IntentFilter filter = new IntentFilter(TransferManager.BROADCAST_ACTION);
        LocalBroadcastManager.getInstance(this).registerReceiver(mFolderBackupReceiver, filter);

        String backupPaths = SettingsManager.instance().getBackupPaths();
        if (!TextUtils.isEmpty(backupPaths)) {
            List<String> pathsList = StringTools.getJsonToList(backupPaths);
            if (pathsList != null) {
                startFolderMonitor(pathsList);
            }
        }
    }

    /**
     * TODO and FIXME:
     * <p>
     * On Android 5.0, API 21 - Android 7.1, API 25, this feature is not available and needs to be refactored
     * <p>
     * Because Apache Commons IO requires a specified JDK version (>=1.8) and other reasons.
     */
    public void startFolderMonitor(List<String> backupPaths) {
        List<FileAlterationObserver> fileAlterationObserverList = new ArrayList<>();

        for (String str : backupPaths) {
            FileAlterationObserver folderFileObserver = new FileAlterationObserver(str);
            folderFileObserver.addListener(new FolderStateChangedListener());
            fileAlterationObserverList.add(folderFileObserver);
        }

        fileMonitor = new FileAlterationMonitor(1000L, fileAlterationObserverList);
        try {
            fileMonitor.start();
        } catch (Exception e) {
            Log.w(DEBUG_TAG, "failed to start file monitor");
            throw new RuntimeException("failed to start file monitor");
        }
    }

    public void stopFolderMonitor() {
        if (fileMonitor != null) {
            try {
                fileMonitor.stop();
            } catch (Exception e) {
                Log.w(DEBUG_TAG, "failed to stop file monitor");
                throw new RuntimeException("failed to stop file monitor");
            }
        }
    }

    public void backupFolder(String email) {
        fileUploaded.clear();
        if (databaseHelper == null) {
            databaseHelper = FolderBackupDBHelper.getDatabaseHelper();
        }

        if (!TextUtils.isEmpty(email)) {
            try {
                repoConfig = databaseHelper.getRepoConfig(email);
            } catch (Exception e) {
                repoConfig = null;
            }
        }

        String backupPaths = SettingsManager.instance().getBackupPaths();
        if (repoConfig == null || TextUtils.isEmpty(backupPaths)) {
            return;
        }
        if (accountManager == null) {
            accountManager = new AccountManager(this);
        }
        currentAccount = accountManager.getCurrentAccount();
        backupPathsList = StringTools.getJsonToList(backupPaths);
        dataManager = new DataManager(currentAccount);

        if (!StringTools.checkFolderUploadNetworkAvailable()) {
//            SeadroidApplication.getInstance().setScanUploadStatus(CameraSyncStatus.NETWORK_UNAVAILABLE);
            return;
        }

        //start backup
        ConcurrentAsyncTask.execute(new FolderBackupTask());
    }

    private class FolderBackupTask extends AsyncTask<Void, Void, String> {

        @Override
        protected String doInBackground(Void... params) {
            for (String str : backupPathsList) {
                String[] split = str.split("/");
                startBackupFolder(split[split.length - 1] + "/", str);
            }
            return null;
        }

        @Override
        protected void onPostExecute(String FilePath) {
            Utils.utilsLogInfo(false, "----------" + FilePath);
        }
    }

    private void startBackupFolder(String parentPath, String filePath) {
        List<FileBean> fileBeanList = new ArrayList<>();
        FileBean fileBean;
        File file = FileTools.getFileByPath(filePath);
        File[] files = file.listFiles();
        if (files != null) {
            for (File value : files) {
                fileBean = new FileBean(value.getAbsolutePath());
                fileBeanList.add(fileBean);
            }
        }
        if (fileBeanList.size() == 0) return;
        for (FileBean fb : fileBeanList) {
            if (fb.isDir()) {
                startBackupFolder(parentPath + fb.getFileName() + "/", fb.getFilePath());
            } else {
                Utils.utilsLogInfo(false, "=relative_path==============" + parentPath + "--------" + fb.getFilePath());

                FolderBackupInfo fileInfo = databaseHelper.getBackupFileInfo(repoConfig.getRepoID(),
                        fb.getFilePath(), fb.getSimpleSize() + "");
                if (fileInfo != null && !TextUtils.isEmpty(fileInfo.filePath)) {
                    Utils.utilsLogInfo(false, "===============" + fileInfo.filePath);
                } else {

                    int taskID = txService.addTaskToSourceQue(Utils.TRANSFER_FOLDER_TAG, currentAccount, repoConfig.getRepoID(),
                            repoConfig.getRepoName(), parentPath, fb.getFilePath(), false, true);
                    if (taskID != 0) {
                        FolderBackupInfo dirInfo = new FolderBackupInfo(repoConfig.getRepoID(), repoConfig.getRepoName(),
                                parentPath, fb.getFileName(), fb.getFilePath(), fb.getSimpleSize() + "");
                        fileUploaded.put(taskID + "", dirInfo);
                    }
                }

            }
        }

    }

    private final ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            synchronized (this) {
                txService = binder.getService();
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            synchronized (this) {
                txService = null;
            }
        }
    };

    @Override
    public void onDestroy() {
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }
        if (mFolderBackupReceiver != null) {
            LocalBroadcastManager.getInstance(this).unregisterReceiver(mFolderBackupReceiver);
        }
        stopFolderMonitor();
        super.onDestroy();
    }

    private class FolderStateChangedListener implements FileAlterationListener {

        public FolderStateChangedListener() {

        }

        @Override
        public void onStart(FileAlterationObserver observer) {

        }

        @Override
        public void onDirectoryCreate(File directory) {
            backupFolders();
        }

        @Override
        public void onDirectoryChange(File directory) {
            backupFolders();
        }

        @Override
        public void onDirectoryDelete(File directory) {

        }

        @Override
        public void onFileCreate(File file) {
            backupFolders();
        }

        @Override
        public void onFileChange(File file) {
            backupFolders();
        }

        @Override
        public void onFileDelete(File file) {

        }

        @Override
        public void onStop(FileAlterationObserver observer) {

        }
    }

    public void backupFolders() {
        String backupEmail = SettingsManager.instance().getBackupEmail();
        if (backupEmail != null) {
            backupFolder(backupEmail);
        }
    }

    private class FolderBackupReceiver extends BroadcastReceiver {

        private FolderBackupReceiver() {
        }

        public void onReceive(Context context, Intent intent) {
            String type = intent.getStringExtra("type");
            if (TextUtils.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_SUCCESS, type)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileBackedUp(taskID);
            }
        }
    }

    private void onFileBackedUp(int taskID) {
        if (fileUploaded != null) {
            FolderBackupInfo uploadInfo = fileUploaded.get(taskID + "");
            if (databaseHelper == null) {
                databaseHelper = FolderBackupDBHelper.getDatabaseHelper();
            }
            if (uploadInfo != null) {
                databaseHelper.saveFileBackupInfo(uploadInfo);
            }
            EventBus.getDefault().post(new FolderBackupEvent("folderBackup"));
        }
    }
}
