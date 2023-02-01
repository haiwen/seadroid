package com.seafile.seadroid2.backupdirectory;

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

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.CameraSyncEvent;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
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


public class FileDirService extends Service{
    private final IBinder mBinder = new FileDirBinder();
    TransferService txService = null;
    private DataManager dataManager;
    private UploadDirectoryDBHelper databaseHelper;
    private AccountManager accountManager;
    private Account currentAccount;
    private RepoInfo repoConfig;
    private List<String> pathList;
    private FolderReceiver mFolderReceiver;

    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }

    public class FileDirBinder extends Binder {
        public FileDirService getService() {
            return FileDirService.this;
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {

        return START_STICKY;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        databaseHelper = UploadDirectoryDBHelper.getDatabaseHelper();
        Intent bIntent = new Intent(this, TransferService.class);
        accountManager = new AccountManager(this);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        if (mFolderReceiver == null) {
            mFolderReceiver = new FolderReceiver();
        }

        IntentFilter filter = new IntentFilter(TransferManager.BROADCAST_ACTION);
        LocalBroadcastManager.getInstance(this).registerReceiver(mFolderReceiver, filter);
    }


    public void uploadFile(String email) {
        if (databaseHelper == null) {
            databaseHelper = UploadDirectoryDBHelper.getDatabaseHelper();
        }
        if (!TextUtils.isEmpty(email)) {
            repoConfig = databaseHelper.getRepoConfig(email);
        }
        String backupPaths = SettingsManager.instance().getBackupPaths();
        if (repoConfig == null || TextUtils.isEmpty(backupPaths)) {
            return;
        }
        if (accountManager == null) {
            accountManager = new AccountManager(this);
        }
        currentAccount = accountManager.getCurrentAccount();
        pathList = StringTools.getDataList(backupPaths);
        dataManager = new DataManager(currentAccount);
        for (String str : pathList) {
            FileDirectoryMonitor fileDirectoryMonitor = new FileDirectoryMonitor();
            FileAlterationObserver fileDirObserver = new FileAlterationObserver(str);
            fileDirObserver.addListener(fileDirectoryMonitor);
            try {
                FileAlterationMonitor fileDirMonitor = new FileAlterationMonitor(1000l);
                fileDirMonitor.addObserver(fileDirObserver);
                fileDirMonitor.start();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        if (!StringTools.checkFolderUploadNetworkAvailable()) {
            // treat dataPlan abort the same way as a network connection error
            SeadroidApplication.getInstance().setScanUploadStatus(CameraSyncStatus.NETWORK_UNAVAILABLE);
            return;
        }
        ConcurrentAsyncTask.execute(new uploadDirTask());

    }

    class uploadDirTask extends AsyncTask<Void, Void, String> {

        @Override
        protected String doInBackground(Void... params) {
            SeafConnection sc = new SeafConnection(currentAccount);
            for (String str : pathList) {
                String[] split = str.split("/");
                try {
                    forceCreateDirectory(dataManager, "/", split[split.length - 1]);
                } catch (SeafException e) {
                    e.printStackTrace();
                }
                isFolder("/" + split[split.length - 1], str);
            }


            return null;
        }

        @Override
        protected void onPostExecute(String directoryFilePath) {
            Utils.utilsLogInfo(false, "----------" + directoryFilePath);

        }
    }

    private void forceCreateDirectory(DataManager dataManager, String parent, String dir) throws SeafException {
        List<SeafDirent> dirs = null;
        if (repoConfig != null && !TextUtils.isEmpty(repoConfig.getRepoID()) && !TextUtils.isEmpty(parent)) {
            dirs = dataManager.getDirentsFromServer(repoConfig.getRepoID(), parent);
        }
        boolean found = false;
        if (dirs == null) return;
        for (SeafDirent dirent : dirs) {
            if (dirent.name.equals(dir) && dirent.isDir()) {
                found = true;
            } else if (dirent.name.equals(dir) && !dirent.isDir()) {
                // there is already a file. move it away.
                String newFilename = getString(R.string.camera_sync_rename_file, dirent.name);
                dataManager.rename(repoConfig.getRepoID(),
                        Utils.pathJoin(Utils.pathJoin("/", parent), dirent.name),
                        newFilename,
                        false);
            }
        }
        if (!found)
            dataManager.createNewDir(repoConfig.getRepoID(), Utils.pathJoin("/", parent), dir);

    }

    private void isFolder(String parentPath, String filePath) {
        List<FileBean> fileBeanList = new ArrayList<>();
        FileBean fileBean;
        File file = FileTools.getFileByPath(filePath);
        File[] files = file.listFiles();
        if (files != null) {
            for (int i = 0; i < files.length; i++) {
                fileBean = new FileBean(files[i].getAbsolutePath(), false);
                fileBeanList.add(fileBean);
            }
        }

        if (fileBeanList == null || fileBeanList.size() == 0) return;

        for (FileBean fb : fileBeanList) {

            if (fb.isDir()) {
                try {
                    forceCreateDirectory(dataManager, parentPath + "/", fb.getFileName());
                } catch (SeafException e) {
                    e.printStackTrace();
                }
                isFolder(parentPath + "/" + fb.getFileName(), fb.getFilePath());
            } else {

                UploadDirInfo fileInfo = databaseHelper.getUploadFileInfo(repoConfig.getRepoID(), fb.getFilePath(), fb.getSimpleSize() + "");
                if (fileInfo != null && !TextUtils.isEmpty(fileInfo.filePath)) {
                    Utils.utilsLogInfo(false, "===============" + fileInfo.filePath);
                } else {
                    int taskID = txService.addTaskToUploadQue(currentAccount,
                            repoConfig.getRepoID(),
                            repoConfig.getRepoName(),
                            parentPath,
                            fb.getFilePath(),
                            false,
                            true);
                    if (taskID != 0) {
                        EventBus.getDefault().post(new CameraSyncEvent("backupFolder", taskID));
                        Utils.utilsLogInfo(false, "isFolder===============" + taskID);
                        UploadDirInfo dirInfo = new UploadDirInfo(repoConfig.getRepoID(), repoConfig.getRepoName(), parentPath, fb.getFileName(), fb.getFilePath(), fb.getSimpleSize() + "");
                        fileUploaded.put(taskID + "", dirInfo);
                    }
                }

            }
        }

    }

    Map<String, UploadDirInfo> fileUploaded = new HashMap<>();

    ServiceConnection mConnection = new ServiceConnection() {
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
        if (mFolderReceiver != null) {
            LocalBroadcastManager.getInstance(this).unregisterReceiver(mFolderReceiver);
        }
        super.onDestroy();

    }

    class FileDirectoryMonitor implements FileAlterationListener {


        public FileDirectoryMonitor() {

        }

        @Override
        public void onStart(FileAlterationObserver observer) {

        }

        @Override
        public void onDirectoryCreate(File directory) {
            uploadFile();

        }

        @Override
        public void onDirectoryChange(File directory) {

        }

        @Override
        public void onDirectoryDelete(File directory) {

        }

        @Override
        public void onFileCreate(File file) {
            uploadFile();
        }

        @Override
        public void onFileChange(File file) {

        }

        @Override
        public void onFileDelete(File file) {

        }

        @Override
        public void onStop(FileAlterationObserver observer) {

        }
    }

    public void uploadFile() {
        String backupEmail = SettingsManager.instance().getBackupEmail();
        if (backupEmail != null) {
            uploadFile(backupEmail);
        }
    }


    private class FolderReceiver extends BroadcastReceiver {

        private FolderReceiver() {
        }

        public void onReceive(Context context, Intent intent) {
            String type = intent.getStringExtra("type");
            if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploaded(taskID);
            } else if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadFailed(taskID);
            }
        }

    }

    private void onFileUploaded(int taskID) {
        Utils.utilsLogInfo(false, "onFileUploaded===============" + taskID);
        if (fileUploaded != null) {
            UploadDirInfo dirInfo = fileUploaded.get(taskID + "");
            if (databaseHelper == null) {
                databaseHelper = UploadDirectoryDBHelper.getDatabaseHelper();
            }
            databaseHelper.saveDirUploadInfo(dirInfo);
        }

    }

    private void onFileUploadFailed(int num) {
        Utils.utilsLogInfo(false, "onFileUploadFailed===============" + num);
    }

}
