package com.seafile.seadroid2.monitor;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationMonitor;
import org.apache.commons.io.monitor.FileAlterationObserver;

import com.seafile.seadroid2.TransferService;
import com.seafile.seadroid2.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.os.Binder;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

public class FileMonitorService extends Service {

    private static final String LOG_TAG = "FileMonitorService";
    public static final String FILEMONITOR = "com.seafile.seadroid2.monitor";
    public static final String FILEPATH = "filepath";
    public static final String ACCOUNTS = "com.seafile.seadroid2.monitor.accounts";
    

    private SeafileMonitor fileMonitor = new SeafileMonitor();
    private TransferService mTransferService;
    private final IBinder mBinder = new MonitorBinder();
    
    private BroadcastReceiver downloadReceiver = new BroadcastReceiver() {

        @Override
        public void onReceive(Context context, Intent intent) {
            
            String type = intent.getStringExtra("type");
            if (type == null) {
                return;
            }
            
            if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_SUCCESS)) {
                
                int taskID = intent.getIntExtra("taskID", 0);
                DownloadTaskInfo info = mTransferService.getDownloadTaskInfo(taskID);
                if (info != null) {
                    SeafCachedFile tmpCachedFile = new SeafCachedFile();
                    tmpCachedFile.repoID = info.repoID;
                    tmpCachedFile.repoName = info.repoName;
                    tmpCachedFile.path = info.path;
                    Account account = info.account;
                    Log.d(LOG_TAG, account.email);
                    fileMonitor.getObserver(account).addToMap(tmpCachedFile);
                }
            }
                      
        }
        
      };
    
    
    
    @Override
    public int onStartCommand(Intent intent, int flags, int startId){
        Log.d(LOG_TAG, "onStartCommand called.");
        
        AccountManager actmgr = new AccountManager(this);
        ArrayList<Account> accounts = new ArrayList<Account>(actmgr.getAccountList());
        addAccounts(accounts);
        fileMonitor.start();
        
        return START_STICKY;
        
    }
    
    
    public class MonitorBinder extends Binder {
        public FileMonitorService getService() {
            return FileMonitorService.this;
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        Log.d(LOG_TAG, "onBind");
        return mBinder;
    }
    
    @Override
    public void onCreate() {
        Log.d(LOG_TAG, "onCreate");
        
        Intent bindIntent = new Intent(this, TransferService.class);
        bindService(bindIntent, mTransferConnection, Context.BIND_AUTO_CREATE);
        
        registerReceiver(downloadReceiver, new IntentFilter(TransferService.BROADCAST_ACTION));
    }

    @Override
    public void onDestroy() {
        Log.d(LOG_TAG, "onDestroy");
        unbindService(mTransferConnection);
        LocalBroadcastManager.getInstance(this).unregisterReceiver(downloadReceiver);
    }
    
    public void addAccounts(ArrayList<Account> accounts) {
        for (int i = 0; i < accounts.size(); ++i) {
            fileMonitor.addAccount(accounts.get(i));
        }
        
    }
    
    public void removeAccount(Account account) {
        Log.d(LOG_TAG, account.email);
        fileMonitor.removeAccount(account);
    }
    
    private ServiceConnection mTransferConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName className, IBinder binder) {
            TransferService.TransferBinder transferBinder = (TransferService.TransferBinder)binder;
            mTransferService = transferBinder.getService();
            LocalBroadcastManager.getInstance(FileMonitorService.this).registerReceiver(downloadReceiver, new IntentFilter(TransferService.BROADCAST_ACTION));
        }

        @Override
        public void onServiceDisconnected(ComponentName className) {
            mTransferService = null;
        }
        
    };
    
    
    public class SeafileObserver implements FileAlterationListener {

        private final String LOG_TAG = "SeafileObserver";
        private Account account;
        private Map<File, SeafCachedFile> fileMap;
        private FileAlterationObserver alterationObserver;
        
        public SeafileObserver(Account account) {
            this.account = account;
            fileMap = new HashMap<File, SeafCachedFile>();
            alterationObserver = new FileAlterationObserver(getAccountDir());
            alterationObserver.addListener(this);
            initFileMap();
        }
        
        private String getAccountDir() {
            DataManager dataManager = new DataManager(account);
            return dataManager.getAccountDir();
        }
        
        public FileAlterationObserver getAlterationObserver() {
            return alterationObserver;
        }
        
        private void initFileMap() {
            DataManager dataManager = new DataManager(account);
            List<SeafCachedFile> cachedfiles = dataManager.getCachedFiles();
            for (SeafCachedFile cached : cachedfiles) {
                File file = dataManager.getLocalRepoFile(cached.repoName, cached.repoID, cached.path);
                if (file.exists()) {
                    fileMap.put(file, cached);
                }
            }
        }
        
        public void addToMap(SeafCachedFile cachedFile) {
            DataManager dataManager = new DataManager(account);
            File file = dataManager.getLocalRepoFile(cachedFile.repoName, cachedFile.repoID, cachedFile.path);
            fileMap.put(file, cachedFile);
        }
        
        public void setAccount(Account account) {
            this.account = account;
        }
        
        public Account getAccount() {
            return account;
        }
        
        public void startWatching() {
            try {
                alterationObserver.initialize();
            } catch (Exception e) {
                
            }
            alterationObserver.checkAndNotify();
        }
        
        public void stopWatching() {
            try {
                alterationObserver.destroy();
            } catch (Exception e) {
                
            }
        }
        
        @Override
        public void onDirectoryChange(File directory) {
            Log.d(LOG_TAG, directory.getPath() + " was modified!");
        }

        @Override
        public void onDirectoryCreate(File directory) {
            Log.d(LOG_TAG, directory.getPath() + " was created!");
        }

        @Override
        public void onDirectoryDelete(File directory) {
            Log.d(LOG_TAG, directory.getPath() + " was deleted!");
        }

        @Override
        public void onFileChange(File file) {
            Log.d(LOG_TAG, file.getPath() + " was modified!");
            SeafCachedFile cachedFile = fileMap.get(file);
            if (cachedFile != null) {
                if (mTransferService != null) {
                    mTransferService.addUploadTask(account,cachedFile.repoID, cachedFile.repoName, Utils.getParentPath(cachedFile.path), file.getPath(), true);
                }
            }
        }

        @Override
        public void onFileCreate(File file) {
            Log.d(LOG_TAG, file.getPath() + " was created!");
        }

        @Override
        public void onFileDelete(File file) {
            Log.d(LOG_TAG, file.getPath() + " was deleted!");
        }

        @Override
        public void onStart(FileAlterationObserver fao) {
            Log.d(LOG_TAG, fao.toString() + " start checking event!");
        }

        @Override
        public void onStop(FileAlterationObserver fao) {
            Log.d(LOG_TAG, fao.toString() + " finished checking event!");
        }
        

    }
    
    public class SeafileMonitor {
        
        private static final String LOG_TAG = "SeafileMonitor";
        private Map<Account, SeafileObserver> observerMap;
        private FileAlterationMonitor alterationMonitor;
        public SeafileMonitor() {
            observerMap = new HashMap<Account, SeafileObserver>();
            alterationMonitor = new FileAlterationMonitor();
        }
        
//        private void recursiveDirectory(String path) {
//            
//            File[] files = new File(path).listFiles();
//            int fileNumber = files.length;
//            for (int i = 0; i < fileNumber; ++i) {
//                if (files[i].isDirectory()) {
//                    observerList.add(new SeafileObserver(files[i].getPath()));
//                    recursiveDirectory(files[i].getPath());
//                } else {
//                    continue;
//                }
//            }
//            
//        }

        
        public void addAccount(Account account) {
            if (observerMap.containsKey(account)) {
                return;
            }
            SeafileObserver fileObserver = new SeafileObserver(account);
            addObserver(fileObserver);
            observerMap.put(account, fileObserver);
        }
        
        public void removeAccount(Account account) {
            
            SeafileObserver fileObserver = observerMap.get(account);
            removeObserver(fileObserver);
            observerMap.remove(account);
            
        }
            
        public void addObserver(SeafileObserver fileObserver) {
            
            alterationMonitor.addObserver(fileObserver.getAlterationObserver());
            
        }
        
        public void removeObserver(SeafileObserver fileObserver) {
            
            alterationMonitor.removeObserver(fileObserver.getAlterationObserver());
            
        }
        
        public SeafileObserver getObserver(Account account) {
            return observerMap.get(account);
        }
        
        public int size() {
            return observerMap.size();
        }
        
        public void start() {
            try {
                alterationMonitor.start();
            } catch (Exception e) {
                Log.d(LOG_TAG, "File Monitor start failed.");
            }
        }
        
        public void stop() {
            try {
                alterationMonitor.stop();
            } catch (Exception e) {
                Log.d(LOG_TAG, "File Monitor stop failed.");
            }
        }
        
    }

}
