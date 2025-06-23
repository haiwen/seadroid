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
import androidx.work.WorkManager;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.download.DownloadFileScannerWorker;
import com.seafile.seadroid2.framework.worker.download.DownloadWorker;
import com.seafile.seadroid2.framework.worker.reupoad.DownloadedFileMonitorWorker;
import com.seafile.seadroid2.framework.worker.upload.FileUploadWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupScanWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupUploadWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupScanWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupUploadWorker;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

public class BackgroundJobManagerImpl {
    private static final String TAG = "BackgroundJobManagerImpl";
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
        SLogs.d(TAG, "oneTimeRequestBuilder()", "Creating WorkRequest for: " + tClass.getSimpleName());

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
                .addTag(TAG_TRANSFER + ":" + tClass.getSimpleName());
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
        SLogs.d(TAG, "startMediaBackupChain()", "isForce:" + isForce);

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

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return oneTimeRequestBuilder(MediaBackupScanWorker.class)
                .setInputData(data)
                .addTag(TAG_ALBUM_BACKUP)
                .setConstraints(constraints)
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
                .addTag(TAG_ALBUM_BACKUP)
                .setId(MediaBackupUploadWorker.UID)
                .build();
    }

    //cancel media
    public void cancelMediaBackupChain() {
        cancelByTag(TAG_ALBUM_BACKUP);
    }

    ////////////////////// upload folder //////////////////////

    public void startFolderBackupChain(boolean isForce) {
        SLogs.d(TAG, "startFolderBackupChain()", "isForce:" + isForce);
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

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return oneTimeRequestBuilder(FolderBackupScanWorker.class)
                .setInputData(data)
                .setId(FolderBackupScanWorker.UID)
                .addTag(TAG_FOLDER_BACKUP)
                .setConstraints(constraints)
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
                .addTag(TAG_FOLDER_BACKUP)
                .setId(FolderBackupUploadWorker.UID)
                .build();
    }

    public void cancelFolderBackupWorker() {
        SLogs.d(TAG, "cancelFolderBackupWorker()");
        cancelByTag(TAG_FOLDER_BACKUP);
    }

    ////////////////////// upload file //////////////////////
    public void startFileUploadWorker() {
        SLogs.d(TAG, "startFileUploadWorker()");

        String workerName = FileUploadWorker.class.getSimpleName();
        OneTimeWorkRequest request = getFileUploadRequest();
        getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, request);
    }

    private OneTimeWorkRequest getFileUploadRequest() {
        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return oneTimeRequestBuilder(FileUploadWorker.class)
                .setId(FileUploadWorker.UID)
                .setConstraints(constraints)
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
        SLogs.d(TAG, "startDownloadChain()");
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

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return oneTimeRequestBuilder(DownloadFileScannerWorker.class)
                .setInputData(builder.build())
                .addTag(TAG_DOWNLOAD)
                .setConstraints(constraints)
                .build();
    }

    private OneTimeWorkRequest getDownloadRequest() {
        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();

        return oneTimeRequestBuilder(DownloadWorker.class)
                .setConstraints(constraints)
                .addTag(TAG_DOWNLOAD)
                .build();
    }

    public void cancelDownloadWorker() {
        cancelByTag(TAG_DOWNLOAD);
    }

    ////////////////////// downloaded file monitor //////////////////////

    public void startCheckDownloadedFileChain() {
        SLogs.d(TAG, "startCheckDownloadedFileChain()");
        OneTimeWorkRequest checkRequest = getCheckDownloadedFileRequest();

        String workerName = DownloadedFileMonitorWorker.class.getSimpleName();
        getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, checkRequest);
    }

    private OneTimeWorkRequest getCheckDownloadedFileRequest() {
        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(NetworkType.CONNECTED)
                .build();
        return oneTimeRequestBuilder(DownloadedFileMonitorWorker.class)
                .setConstraints(constraints)
                .setExpedited(OutOfQuotaPolicy.RUN_AS_NON_EXPEDITED_WORK_REQUEST)
                .build();
    }


    @Deprecated
    public void cancelAllJobs() {
        getWorkManager().cancelAllWork();
    }
}
