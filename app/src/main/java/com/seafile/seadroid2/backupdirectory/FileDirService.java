package com.seafile.seadroid2.backupdirectory;

import android.app.Service;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Binder;
import android.os.IBinder;
import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationMonitor;
import org.apache.commons.io.monitor.FileAlterationObserver;

import java.io.File;
import java.util.ArrayList;
import java.util.List;


public class FileDirService extends Service {
    private final IBinder mBinder = new FileDirBinder();
    TransferService txService = null;
    private String directoryFilePath;
    private DataManager dataManager;
    private Account currentAccount;
    private UploadDirectoryDBHelper databaseHelper;

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
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    private UploadDirConfig dirConfig;

    public void uploadFile(Account account, String directoryFilePath) {

        this.currentAccount = account;
        this.directoryFilePath = directoryFilePath;
        dirConfig = databaseHelper.getDirConfig(account, directoryFilePath);
        if (dirConfig != null) {
            Utils.utilsLogInfo(false, account.getSignature() + "----" + directoryFilePath + "-------0001----new----" + dirConfig.repoName + "------" + dirConfig.repoID);
        }
        dataManager = new DataManager(account);

        FileDirectoryMonitor fileDirectoryMonitor = new FileDirectoryMonitor();
        FileAlterationObserver fileDirObserver = new FileAlterationObserver(directoryFilePath);
        fileDirObserver.addListener(fileDirectoryMonitor);
        try {
            FileAlterationMonitor fileDirMonitor = new FileAlterationMonitor(1000l);
            fileDirMonitor.addObserver(fileDirObserver);
            fileDirMonitor.start();
        } catch (Exception e) {
            e.printStackTrace();
        }
        if ((!SettingsManager.instance().isDirDataPlanAllowed() && Utils.isWiFiOn()) || SettingsManager.instance().isDirDataPlanAllowed()) {
            Utils.utilsLogInfo(false, "--isWiFiOn------true--");
            if (dirConfig != null && !TextUtils.isEmpty(dirConfig.filePath)) {
                ConcurrentAsyncTask.execute(new uploadDirTask());
            }
        } else {
            Utils.utilsLogInfo(false, "--isWiFiOn------false--");
        }

    }

    class uploadDirTask extends AsyncTask<Void, Void, String> {

        @Override
        protected String doInBackground(Void... params) {

            try {
                forceCreateDirectory(dataManager, "/", dirConfig.fileName);
            } catch (SeafException e) {
                e.printStackTrace();
            }
            isFolder("/" + dirConfig.fileName, directoryFilePath);
            return directoryFilePath;
        }

        @Override
        protected void onPostExecute(String directoryFilePath) {
            Utils.utilsLogInfo(false, "----------" + directoryFilePath);

        }
    }

    private void forceCreateDirectory(DataManager dataManager, String parent, String dir) throws SeafException {
        List<SeafDirent> dirs = dataManager.getDirentsFromServer(dirConfig.repoID, parent);
        boolean found = false;
        for (SeafDirent dirent : dirs) {
            if (dirent.name.equals(dir) && dirent.isDir()) {
                found = true;
            } else if (dirent.name.equals(dir) && !dirent.isDir()) {
                // there is already a file. move it away.
                String newFilename = getString(R.string.camera_sync_rename_file, dirent.name);
                dataManager.rename(dirConfig.repoID,
                        Utils.pathJoin(Utils.pathJoin("/", parent), dirent.name),
                        newFilename,
                        false);
            }
        }
        if (!found)
            dataManager.createNewDir(dirConfig.repoID, Utils.pathJoin("/", parent), dir);

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
                if (fileBean.isDir()) {
                    try {
                        forceCreateDirectory(dataManager, parentPath + "/", fileBean.getFileName());
                    } catch (SeafException e) {
                        e.printStackTrace();
                    }
                    isFolder(parentPath + "/" + fileBean.getFileName(), fileBean.getFilePath());
                } else {

                    UploadDirInfo fileInfo = databaseHelper.getUploadFileInfo(currentAccount, dirConfig.repoID, fileBean.getFilePath(), fileBean.getSimpleSize() + "");
                    if (fileInfo != null && !TextUtils.isEmpty(fileInfo.filePath)) {
                        Utils.utilsLogInfo(false, "-----------" + fileInfo.filePath);
                    } else {
                        int num = txService.addTaskToUploadQue(currentAccount,
                                dirConfig.repoID,
                                dirConfig.repoName,
                                parentPath,
                                fileBean.getFilePath(),
                                false,
                                true);
                        if (num != 0) {
                            databaseHelper.saveDirUploadInfo(new UploadDirInfo(currentAccount, dirConfig.repoID, dirConfig.repoName, parentPath, fileBean.getFileName(), fileBean.getFilePath(), fileBean.getSimpleSize() + ""));
                        }
                    }

                }
            }
        }
        BeanListManager.sortFileBeanList(fileBeanList, 0);
    }

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
            if (directory.getParent().equals(directoryFilePath)) {
                uploadFile();
                Utils.utilsLogInfo(false, directory.getName() + "--11111-----onDirectoryCreate---" + directory.getAbsolutePath() + "----" + directory.getParent());
            }

        }

        @Override
        public void onDirectoryChange(File directory) {
            Utils.utilsLogInfo(false, directory.getParent() + "---11111----onDirectoryChange----" + directory.getAbsolutePath());
            uploadFile();
        }

        @Override
        public void onDirectoryDelete(File directory) {
            Utils.utilsLogInfo(false, "----11111---onDirectoryDelete-----");
//            uploadFile();
        }

        @Override
        public void onFileCreate(File file) {

            if (file.getParent().equals(directoryFilePath)) {
                uploadFile();
                Utils.utilsLogInfo(false, file.getName() + "---11111----onFileCreate-------" + file.getPath() + "====" + file.getParent());
            }
        }

        @Override
        public void onFileChange(File file) {
            Utils.utilsLogInfo(false, file.getName() + "---11111----onFileChange--" + file.getPath() + "-----" + file.getAbsolutePath());
            uploadFile();
        }

        @Override
        public void onFileDelete(File file) {
            Utils.utilsLogInfo(false, "----11111---onFileDelete--------");
        }

        @Override
        public void onStop(FileAlterationObserver observer) {

        }
    }

    public void uploadFile() {
        Account account = dataManager.getAccount();
        String dirPath = SettingsManager.instance().getDirectoryFilePath();
        Utils.utilsLogInfo(false, dataManager.getAccount() + "-----------" + dirPath);
        uploadFile(account, dirPath);

    }

}
