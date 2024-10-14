package com.seafile.seadroid2.framework.worker;

import android.text.TextUtils;

import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.ExistingWorkPolicy;
import androidx.work.ListenableWorker;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkInfo;
import androidx.work.WorkManager;

import com.google.common.util.concurrent.ListenableFuture;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.download.DownloadFileScanWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadedFileMonitorWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadFileManuallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadMediaFileAutomaticallyWorker;

import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

public class BackgroundJobManagerImpl {
    public static final String TAG_ALL = "*";
    public static final String TAG_TRANSFER = TAG_ALL + ":transfer";

    private final long MAX_CONTENT_TRIGGER_DELAY_MS = 1500L;
    private final long PERIODIC_BACKUP_INTERVAL_MINUTES = 24 * 60L;
    private final long DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES = 15L;

    private BackgroundJobManagerImpl() {

    }

    public static BackgroundJobManagerImpl getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static class SingletonHolder {
        private static final BackgroundJobManagerImpl INSTANCE = new BackgroundJobManagerImpl();
    }

    private <T extends ListenableWorker> OneTimeWorkRequest.Builder oneTimeRequestBuilder(Class<T> tClass) {
        return new OneTimeWorkRequest.Builder(tClass)
                .addTag(TAG_ALL)
                .addTag(TAG_TRANSFER)
                .addTag(tClass.getSimpleName());
    }

    private <T extends ListenableWorker> PeriodicWorkRequest.Builder periodicRequestBuilder(Class<T> tClass, long intervalMins, long flexIntervalMins) {
        if (intervalMins == 0) {
            intervalMins = DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES;
        }
        if (flexIntervalMins == 0) {
            flexIntervalMins = DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES;
        }
        return new PeriodicWorkRequest.Builder(tClass, intervalMins, TimeUnit.MINUTES, flexIntervalMins, TimeUnit.MINUTES)
                .addTag(TAG_ALL)
                .addTag(TAG_TRANSFER)
                .addTag(tClass.getSimpleName());
    }

    private boolean checkWorkerIsRunningById(UUID uid) {
        ListenableFuture<WorkInfo> listenableFuture = getWorkManager().getWorkInfoById(uid);
        try {
            WorkInfo task = listenableFuture.get();
            if (task == null) {
                return false;
            }

            return !task.getState().isFinished();
        } catch (ExecutionException | InterruptedException e) {
            SLogs.e("checkWorkerIsRunningById", e);
            return false;
        }

    }

    public void cancelById(UUID uid) {
        getWorkManager().cancelWorkById(uid);
    }

    public WorkInfo getWorkInfoById(UUID uid) {
        ListenableFuture<WorkInfo> listener = getWorkManager().getWorkInfoById(uid);
        try {
            return listener.get();
        } catch (ExecutionException | InterruptedException e) {
            SLogs.w("getWorkInfoById", e);
            return null;
        }
    }

    public WorkManager getWorkManager() {
        return WorkManager.getInstance(SeadroidApplication.getAppContext());
    }

    ///////////////////
    /// media worker
    ///////////////////
    public void startMediaChainWorker(boolean isForce) {
        cancelAllMediaWorker();

        OneTimeWorkRequest scanRequest = getMediaScanRequest(isForce);
        OneTimeWorkRequest uploadRequest = getMediaUploadRequest();

        String workerName = MediaBackupScannerWorker.class.getSimpleName();


        getWorkManager()
                .beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, scanRequest)
                .then(uploadRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getMediaScanRequest(boolean isForce) {
        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        return oneTimeRequestBuilder(MediaBackupScannerWorker.class)
                .setInputData(data)
                .setInitialDelay(1, TimeUnit.SECONDS)
                .setId(MediaBackupScannerWorker.UID)
                .build();
    }

    private OneTimeWorkRequest getMediaUploadRequest() {
        NetworkType networkType = NetworkType.UNMETERED;
        if (AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch()) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        return oneTimeRequestBuilder(UploadMediaFileAutomaticallyWorker.class)
                .setConstraints(constraints)
                .setInitialDelay(1, TimeUnit.SECONDS)
                .setId(UploadMediaFileAutomaticallyWorker.UID)
                .build();
    }

    //cancel media
    public void cancelAllMediaWorker() {
        cancelById(UploadMediaFileAutomaticallyWorker.UID);
        cancelById(MediaBackupScannerWorker.UID);
    }

    ///////////////////
    /// upload folder
    ///////////////////
    public void startFolderChainWorker(boolean isForce) {
        cancelAllFolderUploadWorker();

        OneTimeWorkRequest scanRequest = getFolderScanRequest(isForce);
        OneTimeWorkRequest uploadRequest = getFolderUploadRequest();

        String workerName = FolderBackupScannerWorker.class.getSimpleName();

        getWorkManager()
                .beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, scanRequest)
                .then(uploadRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getFolderScanRequest(boolean isForce) {
        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        return oneTimeRequestBuilder(FolderBackupScannerWorker.class)
                .setInputData(data)
                .setInitialDelay(1, TimeUnit.SECONDS)
                .setId(FolderBackupScannerWorker.UID)
                .build();
    }

    private OneTimeWorkRequest getFolderUploadRequest() {
        NetworkType networkType = NetworkType.UNMETERED;
        if (FolderBackupSharePreferenceHelper.readDataPlanAllowed()) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        return oneTimeRequestBuilder(UploadFolderFileAutomaticallyWorker.class)
                .setConstraints(constraints)
                .setInitialDelay(1, TimeUnit.SECONDS)
                .setId(UploadFolderFileAutomaticallyWorker.UID)
                .build();
    }

    public void cancelAllFolderUploadWorker() {
        cancelById(FolderBackupScannerWorker.UID);
        cancelById(UploadFolderFileAutomaticallyWorker.UID);
    }

    ///////////////////
    /// upload file
    ///////////////////
    public void startFileUploadWorker() {
        String workerName = UploadFileManuallyWorker.class.getSimpleName();
        OneTimeWorkRequest request = getFileUploadRequest();
        getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, request);
    }

    private OneTimeWorkRequest getFileUploadRequest() {
        return oneTimeRequestBuilder(UploadFileManuallyWorker.class)
                .setId(UploadFileManuallyWorker.UID)
                .build();
    }

    ///////////////////
    /// download
    ///////////////////
    public OneTimeWorkRequest getDownloadScanRequest(String transferId, String[] direntIds) {
        Data.Builder builder = new Data.Builder();
        if (!TextUtils.isEmpty(transferId)) {
            builder.putString(DownloadFileScanWorker.DATA_TRANSFER_ID_KEY, transferId);
        }
        if (direntIds != null && direntIds.length > 0) {
            builder.putStringArray(TransferWorker.DATA_DIRENT_LIST_KEY, direntIds);
        }

        return oneTimeRequestBuilder(DownloadFileScanWorker.class)
                .setInputData(builder.build())
                .build();
    }

    private OneTimeWorkRequest getDownloadRequest() {
        return oneTimeRequestBuilder(DownloadWorker.class)
                .setId(DownloadWorker.UID)
                .build();
    }

    public void startDownloadChainWorker() {
        startDownloadChainWorker(null, null);
    }

    /**
     * in batches
     */
    public void startDownloadChainWorker(String[] direntIds) {
        startDownloadChainWorker(null, direntIds);
    }

    public void startDownloadChainWorker(String transferId) {
        startDownloadChainWorker(transferId, null);
    }

    public void startDownloadChainWorker(String transferId, String[] direntIds) {

        OneTimeWorkRequest scanRequest = getDownloadScanRequest(transferId, direntIds);
        OneTimeWorkRequest downloadRequest = getDownloadRequest();

        String workerName = DownloadFileScanWorker.class.getSimpleName();
        getWorkManager().beginUniqueWork(workerName, ExistingWorkPolicy.REPLACE, scanRequest)
                .then(downloadRequest)
                .enqueue();
    }

    public void startCheckDownloadedFileChainWorker(String filePath) {

        OneTimeWorkRequest fileRequest = getFileUploadRequest();
        OneTimeWorkRequest checkRequest = getCheckDownloadedFileRequest(filePath);

        String workerName = DownloadedFileMonitorWorker.class.getSimpleName();

        getWorkManager().beginUniqueWork(workerName, ExistingWorkPolicy.REPLACE, checkRequest)
                .then(fileRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getCheckDownloadedFileRequest(String filePath) {
        Data data = new Data.Builder()
                .putString(DownloadedFileMonitorWorker.FILE_CHANGE_KEY, filePath)
                .build();

        return oneTimeRequestBuilder(DownloadedFileMonitorWorker.class)
                .setInputData(data)
                .build();
    }


    public void cancelDownloadWorker() {
        cancelById(DownloadWorker.UID);
        cancelById(DownloadedFileMonitorWorker.UID);
        cancelById(DownloadFileScanWorker.UID);
    }

    public void cancelAllJobs() {
        getWorkManager().cancelAllWork();
    }


}
