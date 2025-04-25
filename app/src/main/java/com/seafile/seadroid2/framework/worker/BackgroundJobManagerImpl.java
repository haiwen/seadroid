package com.seafile.seadroid2.framework.worker;

import android.text.TextUtils;

import androidx.work.BackoffPolicy;
import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.ExistingWorkPolicy;
import androidx.work.ListenableWorker;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.OutOfQuotaPolicy;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkInfo;
import androidx.work.WorkManager;

import com.google.common.util.concurrent.ListenableFuture;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.download.DownloadFileScannerWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadedFileMonitorWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupScanWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupScanWorker;
import com.seafile.seadroid2.framework.worker.upload.FileUploadWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupUploadWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupUploadWorker;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

public class BackgroundJobManagerImpl {
    public static final String TAG_ALL = "*";
    public static final String TAG_TRANSFER = TAG_ALL + ":transfer";
    public static final String TAG_ALBUM_BACKUP = TAG_TRANSFER + ":album_backup";
    public static final String TAG_FOLDER_BACKUP = TAG_TRANSFER + ":folder_backup";
    public static final String TAG_FILE_UPLOAD = TAG_TRANSFER + ":file_upload";
    public static final String TAG_DOWNLOAD = TAG_TRANSFER + ":download";

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
                .setBackoffCriteria(BackoffPolicy.LINEAR, 5, TimeUnit.SECONDS)
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

    public void cancelById(UUID uid) {
        getWorkManager().cancelWorkById(uid);
    }

    public void cancelByTag(String tag) {
        if (TextUtils.isEmpty(tag)) {
            return;
        }

        getWorkManager().cancelAllWorkByTag(tag);
    }

    public WorkManager getWorkManager() {
        return WorkManager.getInstance(SeadroidApplication.getAppContext());
    }

    ////////////////////// media //////////////////////

    public void startMediaBackupChain(boolean isForce) {
        cancelMediaBackupChain();

        OneTimeWorkRequest scanRequest = getMediaScannerWorkerRequest(isForce);
        OneTimeWorkRequest uploadRequest = getMediaUploadWorkerRequest();

        String workerName = MediaBackupScanWorker.class.getSimpleName();

        getWorkManager()
                .beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, scanRequest)
                .then(uploadRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getMediaScannerWorkerRequest(boolean isForce) {
        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        return oneTimeRequestBuilder(MediaBackupScanWorker.class)
                .setInputData(data)
                .setId(MediaBackupScanWorker.UID)
                .build();
    }

    private OneTimeWorkRequest getMediaUploadWorkerRequest() {
        NetworkType networkType = NetworkType.UNMETERED;
        boolean isAllowData = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        if (isAllowData) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        return oneTimeRequestBuilder(MediaBackupUploadWorker.class)
                .setConstraints(constraints)
                .setId(MediaBackupUploadWorker.UID)
                .build();
    }

    public void restartMediaBackupWorker() {
        cancelByTag(TAG_ALBUM_BACKUP);
        startMediaBackupChain(false);
    }

    //cancel media
    public void cancelMediaBackupChain() {
        cancelByTag(TAG_ALBUM_BACKUP);
    }

    ////////////////////// upload folder //////////////////////

    public void startFolderBackupChain(boolean isForce) {
        cancelFolderBackupWorker();

        OneTimeWorkRequest scanRequest = getFolderBackupScanWorkerRequest(isForce);
        OneTimeWorkRequest uploadRequest = getFolderBackupUploadWorkerRequest();

        String workerName = FolderBackupScanWorker.class.getSimpleName();

        getWorkManager()
                .beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, scanRequest)
                .then(uploadRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getFolderBackupScanWorkerRequest(boolean isForce) {
        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        return oneTimeRequestBuilder(FolderBackupScanWorker.class)
                .setInputData(data)
                .setId(FolderBackupScanWorker.UID)
                .build();
    }

    private OneTimeWorkRequest getFolderBackupUploadWorkerRequest() {
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

        return oneTimeRequestBuilder(FolderBackupUploadWorker.class)
                .setConstraints(constraints)
                .setId(FolderBackupUploadWorker.UID)
                .build();
    }

    public void restartFolderBackupWorker() {
        cancelByTag(TAG_FOLDER_BACKUP);

        startFolderBackupChain(false);
    }

    public void cancelFolderBackupWorker() {
        cancelByTag(TAG_FOLDER_BACKUP);
    }

    ////////////////////// upload file //////////////////////
    public void startFileUploadWorker() {
        String workerName = FileUploadWorker.class.getSimpleName();
        OneTimeWorkRequest request = getFileUploadRequest();
        getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, request);
    }

    private OneTimeWorkRequest getFileUploadRequest() {
        return oneTimeRequestBuilder(FileUploadWorker.class)
                .setId(FileUploadWorker.UID)
                .setExpedited(OutOfQuotaPolicy.RUN_AS_NON_EXPEDITED_WORK_REQUEST)
                .addTag(TAG_FILE_UPLOAD)
                .build();
    }

    public void cancelFileUploadWorker() {
        cancelByTag(TAG_FILE_UPLOAD);
    }


    ////////////////////// download //////////////////////

    public void startDownloadChain() {
        startDownloadChain(null);
    }

    public void startDownloadChain(List<String> direntIds) {
        OneTimeWorkRequest scanRequest = getDownloadScanRequest(direntIds);
        OneTimeWorkRequest downloadRequest = getDownloadRequest();

        String workerName = DownloadFileScannerWorker.class.getSimpleName();
        getWorkManager().beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, scanRequest)
                .then(downloadRequest)
                .enqueue();
    }

    public void startDownloadWorker() {
        OneTimeWorkRequest downloadRequest = getDownloadRequest();

        String workerName = DownloadWorker.class.getSimpleName();
        getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, downloadRequest);
    }

    public OneTimeWorkRequest getDownloadScanRequest(List<String> direntIds) {
        Data.Builder builder = new Data.Builder();

        if (direntIds != null && !direntIds.isEmpty()) {
            builder.putString(TransferWorker.DATA_DIRENT_LIST_KEY, String.join(",", direntIds));
        }

        return oneTimeRequestBuilder(DownloadFileScannerWorker.class)
                .setInputData(builder.build())
                .addTag(TAG_DOWNLOAD)
                .build();
    }

    private OneTimeWorkRequest getDownloadRequest() {
        return oneTimeRequestBuilder(DownloadWorker.class)
                .addTag(TAG_DOWNLOAD)
                .build();
    }


    public void cancelDownloadWorker() {
        cancelByTag(TAG_DOWNLOAD);
    }

    ////////////////////// downloaded file monitor //////////////////////

    public void startCheckDownloadedFileChain() {
        OneTimeWorkRequest checkRequest = getCheckDownloadedFileRequest();
        OneTimeWorkRequest uploadRequest = getFileUploadRequest();

        String workerName = DownloadedFileMonitorWorker.class.getSimpleName();

        getWorkManager()
                .beginUniqueWork(workerName, ExistingWorkPolicy.KEEP, checkRequest)
                .then(uploadRequest)
                .enqueue();
    }

    private OneTimeWorkRequest getCheckDownloadedFileRequest() {
        return oneTimeRequestBuilder(DownloadedFileMonitorWorker.class).build();
    }


    public void cancelAllJobs() {
        getWorkManager().cancelAllWork();
    }


}
