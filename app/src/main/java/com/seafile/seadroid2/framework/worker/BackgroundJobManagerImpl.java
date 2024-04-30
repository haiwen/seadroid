package com.seafile.seadroid2.framework.worker;

import android.annotation.SuppressLint;

import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.ExistingWorkPolicy;
import androidx.work.ListenableWorker;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.common.collect.Lists;
import com.google.common.util.concurrent.ListenableFuture;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import io.reactivex.Completable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Action;
import io.reactivex.schedulers.Schedulers;

public class BackgroundJobManagerImpl {
    public static final String TAG_ALL = "*";
    public static final String TAG_TRANSFER = TAG_ALL + ":transfer";

    public static final String TAG_TRANSFER_DOWNLOAD = TAG_TRANSFER + ":download";
    public static final String TAG_TRANSFER_UPLOAD = TAG_TRANSFER + ":upload";

    public static final String TAG_TRANSFER_DOWNLOAD_CHECKER = TAG_TRANSFER + ":download_checker";


    //SYNC
    public static final String TAG_TRANSFER_DOWNLOAD_SCAN = TAG_TRANSFER + ":download_scan";
    public static final String TAG_TRANSFER_UPLOAD_SCAN = TAG_TRANSFER + ":upload_scan";

    //download
    public static final String TAG_TRANSFER_DOWNLOAD_FILES_SCAN = TAG_TRANSFER_DOWNLOAD_SCAN + ":schedule_files_download_scan_worker";
    public static final String TAG_TRANSFER_DOWNLOAD_FILES_WORKER = TAG_TRANSFER_DOWNLOAD + ":download_files_worker";

    //upload
    public static final String TAG_TRANSFER_UPLOAD_FOLDER_SCAN = TAG_TRANSFER_UPLOAD_SCAN + ":schedule_folder_upload_scan_worker";
    public static final String TAG_TRANSFER_UPLOAD_FOLDER_BACKUP_WORKER = TAG_TRANSFER_UPLOAD + ":upload_folder_backup_worker";
    public static final String TAG_TRANSFER_UPLOAD_FILE_BACKUP_WORKER = TAG_TRANSFER_UPLOAD + ":upload_file_backup_worker";


    //media
    public static final String TAG_TRANSFER_UPLOAD_MEDIA_SCAN = TAG_TRANSFER_UPLOAD_SCAN + ":schedule_media_upload_scan_worker";
    public static final String TAG_TRANSFER_UPLOAD_MEDIA_WORKER = TAG_TRANSFER_UPLOAD + ":upload_media_worker";


    public static final String JOB_CONTENT_OBSERVER = "content_observer";
    public static final String JOB_PERIODIC_MEDIA_DETECTION = "periodic_media_detection";

    public static final String JOB_NOTIFICATION = "notification";

    public static final String JOB_PERIODIC_HEALTH_STATUS = "periodic_health_status";


    private final String TAG_PREFIX_NAME = "name";

    private final long MAX_CONTENT_TRIGGER_DELAY_MS = 1500L;
    private final long PERIODIC_BACKUP_INTERVAL_MINUTES = 24 * 60L;
    private final long DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES = 15L;

    private BackgroundJobManagerImpl() {

    }

    private List<Disposable> disposableList = Lists.newArrayList();

    public static BackgroundJobManagerImpl getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static class SingletonHolder {
        private static final BackgroundJobManagerImpl INSTANCE = new BackgroundJobManagerImpl();
    }

    private <T extends ListenableWorker> OneTimeWorkRequest.Builder oneTimeRequestBuilder(Class<T> tClass, String jobName) {
        return new OneTimeWorkRequest.Builder(tClass)
                .addTag(TAG_ALL)
                .addTag(jobName);
    }

    private <T extends ListenableWorker> PeriodicWorkRequest.Builder periodicRequestBuilder(Class<T> tClass, String jobName, long intervalMins, long flexIntervalMins) {
        if (intervalMins == 0) {
            intervalMins = DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES;
        }
        if (flexIntervalMins == 0) {
            flexIntervalMins = DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES;
        }
        return new PeriodicWorkRequest.Builder(tClass, intervalMins, TimeUnit.MINUTES, flexIntervalMins, TimeUnit.MINUTES)
                .addTag(TAG_ALL)
                .addTag(jobName);
    }

    private boolean checkWorkerIsRunningById(UUID uid) {
        ListenableFuture<WorkInfo> listenableFuture = SupportWorkManager.getWorkManager().getWorkInfoById(uid);
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

    public WorkInfo getWorkInfoById(UUID uid) {
        ListenableFuture<WorkInfo> listener = SupportWorkManager.getWorkManager().getWorkInfoById(uid);
        try {
            return listener.get();
        } catch (ExecutionException | InterruptedException e) {
            SLogs.w("getWorkInfoById", e);
            return null;
        }

    }

    private boolean checkWorkerIsRunningByTag(String tag) {
        ListenableFuture<List<WorkInfo>> workInfos = SupportWorkManager.getWorkManager().getWorkInfosByTag(tag);
        try {
            List<WorkInfo> workInfosList = workInfos.get();
            if (CollectionUtils.isEmpty(workInfosList)) {
                return false;
            }

            boolean isFinish = workInfosList.get(0).getState().isFinished();

            return !isFinish;
        } catch (ExecutionException | InterruptedException e) {
            SLogs.e("checkWorkerIsRunningByTag", e);
            return false;
        }
    }

    private Completable startWorkerUntilStopped(String workTag) {
        return Completable.fromAction(() -> {
                    while (true) {
                        boolean isRunning = checkWorkerIsRunningByTag(workTag);
                        if (isRunning) {
                            SLogs.d(workTag + " is running");
                            Thread.sleep(500);
                        }

                        SLogs.d(workTag + " is stopped");
                        break;
                    }
                }).subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }

    ///////////////////media///////////////////
    public void scheduleMediaScanWorker(boolean isForce) {
        boolean isRunning = checkWorkerIsRunningById(MediaBackupScannerWorker.UID);
        if (isRunning) {
            SLogs.d(TAG_TRANSFER_UPLOAD_MEDIA_SCAN + " is running");
            return;
        }

        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(MediaBackupScannerWorker.class, TAG_TRANSFER_UPLOAD_MEDIA_SCAN)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_UPLOAD_SCAN)
                .setInputData(data)
                .setId(MediaBackupScannerWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_TRANSFER_UPLOAD_MEDIA_SCAN, ExistingWorkPolicy.KEEP, request);
    }

    public void startMediaBackupWorker() {
        boolean isRunning = checkWorkerIsRunningById(UploadMediaFileAutomaticallyWorker.UID);
        if (isRunning) {
            SLogs.d(TAG_TRANSFER_UPLOAD_MEDIA_WORKER + " is running");
        }

        NetworkType networkType = NetworkType.UNMETERED;
        if (AlbumBackupManager.readAllowDataPlanSwitch()) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadMediaFileAutomaticallyWorker.class, TAG_TRANSFER_UPLOAD_MEDIA_WORKER)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_UPLOAD)
                .setConstraints(constraints)
                .setId(UploadMediaFileAutomaticallyWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_TRANSFER_UPLOAD_MEDIA_WORKER, ExistingWorkPolicy.REPLACE, request);
    }

    //cancel media
    public void cancelMediaWorker() {
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_TRANSFER_UPLOAD_MEDIA_SCAN);
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_TRANSFER_UPLOAD_MEDIA_WORKER);
    }

    public void restartMediaUploadWorker(boolean isForce) {
        cancelMediaWorker();

        Disposable disposable = startWorkerUntilStopped(TAG_TRANSFER_UPLOAD_MEDIA_WORKER).subscribe(new Action() {
            @Override
            public void run() throws Exception {
                scheduleMediaScanWorker(isForce);
            }
        });

        disposableList.add(disposable);
    }


    ///////////////////upload folder///////////////////
    public void scheduleFolderBackupScannerWorker(boolean isForce) {
        boolean isRunning = checkWorkerIsRunningById(FolderBackupScannerWorker.UID);
        if (isRunning) {
            SLogs.w(FolderBackupScannerWorker.class.getSimpleName() + " is running");
        }

        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(FolderBackupScannerWorker.class, TAG_TRANSFER_UPLOAD_FOLDER_SCAN)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_UPLOAD_SCAN)
                .setInputData(data)
                .setId(FolderBackupScannerWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_TRANSFER_UPLOAD_FOLDER_SCAN, ExistingWorkPolicy.KEEP, request);
    }


    public void cancelFolderWorker() {
        SupportWorkManager.getWorkManager().cancelWorkById(FolderBackupScannerWorker.UID);
        SupportWorkManager.getWorkManager().cancelWorkById(UploadFolderFileAutomaticallyWorker.UID);
    }

    public void restartFolderUploadWorker(NetworkType networkType) {
        cancelFolderWorker();

        Disposable disposable = startWorkerUntilStopped(TAG_TRANSFER_UPLOAD_FOLDER_BACKUP_WORKER).subscribe(new Action() {
            @Override
            public void run() throws Exception {
                startFolderUploadWorker(networkType);
            }
        });

        disposableList.add(disposable);
    }

    public void startFolderUploadWorker() {
        NetworkType networkType = NetworkType.UNMETERED;
        if (FolderBackupManager.readDataPlanAllowed()) {
            networkType = NetworkType.CONNECTED;
        }
        startFolderUploadWorker(networkType);
    }

    public void startFolderUploadWorker(NetworkType networkType) {
        boolean isRunning = checkWorkerIsRunningById(UploadFolderFileAutomaticallyWorker.UID);
        if (isRunning) {
            SLogs.w(UploadFolderFileAutomaticallyWorker.class.getSimpleName() + " is running");
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadFolderFileAutomaticallyWorker.class, TAG_TRANSFER_UPLOAD_FOLDER_BACKUP_WORKER)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_UPLOAD)
                .setConstraints(constraints)
                .setId(UploadFolderFileAutomaticallyWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_TRANSFER_UPLOAD_FOLDER_BACKUP_WORKER, ExistingWorkPolicy.REPLACE, request);
    }


    //
    public void cancelFilesUploadWorker() {
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_TRANSFER_UPLOAD_FOLDER_BACKUP_WORKER);
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_TRANSFER_UPLOAD_FOLDER_SCAN);
    }

    ///////////////////upload file///////////////////
    public void startFileUploadWorker() {
        boolean isRunning = checkWorkerIsRunningById(UploadFileManuallyWorker.UID);
        if (isRunning) {
            SLogs.w(UploadFileManuallyWorker.class.getSimpleName() + " is running");
        }

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadFileManuallyWorker.class, TAG_TRANSFER_UPLOAD_FILE_BACKUP_WORKER)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_UPLOAD)
                .setId(UploadFileManuallyWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_TRANSFER_UPLOAD_FILE_BACKUP_WORKER, ExistingWorkPolicy.KEEP, request);
    }


    ///////////////////download///////////////////
    public void scheduleOneTimeFilesDownloadScanWorker() {
        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileScanWorker.class, TAG_TRANSFER_DOWNLOAD_FILES_SCAN)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_DOWNLOAD_SCAN)
                .build();
        SupportWorkManager.getWorkManager().enqueue(request);
    }

    public void scheduleOneTimeFilesDownloadScanWorker(String[] direntIds) {
        Data data = new Data.Builder()
                .putStringArray(TransferWorker.DATA_DIRENT_LIST_KEY, direntIds)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileScanWorker.class, TAG_TRANSFER_DOWNLOAD_FILES_SCAN)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_DOWNLOAD_SCAN)
                .setInputData(data)
                .build();
        SupportWorkManager.getWorkManager().enqueue(request);
    }

    public void scheduleOneTimeFilesDownloadScanWorker(String transferId) {
        Data data = new Data.Builder()
                .putString(DownloadFileScanWorker.DATA_TRANSFER_KEY, transferId)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileScanWorker.class, TAG_TRANSFER_DOWNLOAD_FILES_SCAN)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_DOWNLOAD_SCAN)
                .setInputData(data)
                .build();
        SupportWorkManager.getWorkManager().enqueue(request);
    }


    public void startFileDownloadWorker() {
        boolean isRunning = checkWorkerIsRunningByTag(TAG_TRANSFER_DOWNLOAD_FILES_WORKER);
        if (isRunning) {
            SLogs.w(TAG_TRANSFER_DOWNLOAD_FILES_WORKER + " is running");
            return;
        }

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadWorker.class, TAG_TRANSFER_DOWNLOAD_FILES_WORKER)
                .addTag(TAG_TRANSFER)
                .addTag(TAG_TRANSFER_DOWNLOAD)
                .setId(DownloadWorker.UID)
                .build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_TRANSFER_DOWNLOAD_FILES_WORKER, ExistingWorkPolicy.KEEP, request);
    }

    public void startDownloadCheckerWorker(String filePath) {
        Data data = new Data.Builder()
                .putString(DownloadedFileCheckerWorker.FILE_CHANGE_KEY, filePath)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadedFileCheckerWorker.class, TAG_TRANSFER_DOWNLOAD_CHECKER)
                .addTag(TAG_TRANSFER)
                .setInputData(data)
                .build();

        SupportWorkManager.getWorkManager().enqueue(request);
    }


    public void cancelFilesDownloadJob() {
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_TRANSFER_DOWNLOAD_FILES_SCAN);
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_TRANSFER_DOWNLOAD_FILES_WORKER);
    }


    public void cancelAllJobs() {
        for (Disposable disposable : disposableList) {
            if (!disposable.isDisposed()) {
                disposable.dispose();
            }
        }

        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_ALL);
    }


}
