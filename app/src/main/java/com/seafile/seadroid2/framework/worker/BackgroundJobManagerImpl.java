package com.seafile.seadroid2.framework.worker;

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

    public static final String JOB_CONTENT_OBSERVER = "content_observer";
    public static final String JOB_PERIODIC_MEDIA_DETECTION = "periodic_media_detection";

    public static final String JOB_NOTIFICATION = "notification";

    public static final String JOB_PERIODIC_HEALTH_STATUS = "periodic_health_status";

    private final long MAX_CONTENT_TRIGGER_DELAY_MS = 1500L;
    private final long PERIODIC_BACKUP_INTERVAL_MINUTES = 24 * 60L;
    private final long DEFAULT_PERIODIC_JOB_INTERVAL_MINUTES = 15L;

    private BackgroundJobManagerImpl() {

    }

    private final List<Disposable> disposableList = Lists.newArrayList();

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

    public void cancelById(UUID uid) {
        SupportWorkManager.getWorkManager().cancelWorkById(uid);
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

    private Completable startWorkerUntilStopped(UUID uid) {
        return Completable.fromAction(() -> {
                    while (true) {
                        boolean isRunning = checkWorkerIsRunningById(uid);
                        if (isRunning) {
                            SLogs.d(uid + " is running");
                            Thread.sleep(100);
                        }

                        SLogs.d(uid + " is stopped");
                        break;
                    }
                }).subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }

    ///////////////////media///////////////////
    public void scheduleMediaScanWorker(boolean isForce) {

        String workerName = MediaBackupScannerWorker.class.getSimpleName();

        boolean isRunning = checkWorkerIsRunningById(MediaBackupScannerWorker.UID);
        if (isRunning) {
            SLogs.d(workerName + " is running");
            return;
        }

        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(MediaBackupScannerWorker.class)
                .setInputData(data)
                .setId(MediaBackupScannerWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, request);
    }

    public void startMediaBackupWorker() {
        String workerName = UploadMediaFileAutomaticallyWorker.class.getSimpleName();
        boolean isRunning = checkWorkerIsRunningById(UploadMediaFileAutomaticallyWorker.UID);
        if (isRunning) {
            SLogs.d(workerName + " is running");
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

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadMediaFileAutomaticallyWorker.class)
                .setConstraints(constraints)
                .setId(UploadMediaFileAutomaticallyWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.REPLACE, request);
    }

    //cancel media
    public void cancelMediaWorker() {
        cancelById(UploadMediaFileAutomaticallyWorker.UID);
        cancelById(MediaBackupScannerWorker.UID);
    }

    public void restartMediaBackupWorker(boolean isForce) {
        cancelMediaWorker();

        Disposable disposable = startWorkerUntilStopped(UploadMediaFileAutomaticallyWorker.UID).subscribe(new Action() {
            @Override
            public void run() throws Exception {
                scheduleMediaScanWorker(isForce);
            }
        });

        disposableList.add(disposable);
    }


    ///////////////////upload folder///////////////////
    public void scheduleFolderBackupScannerWorker(boolean isForce) {
        String workerName = FolderBackupScannerWorker.class.getSimpleName();
        boolean isRunning = checkWorkerIsRunningById(FolderBackupScannerWorker.UID);
        if (isRunning) {
            SLogs.w(workerName + " is running");
        }

        Data data = new Data.Builder()
                .putBoolean(TransferWorker.DATA_FORCE_TRANSFER_KEY, isForce)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(FolderBackupScannerWorker.class)
                .setInputData(data)
                .setId(FolderBackupScannerWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, request);
    }


    public void cancelFolderWorker() {
        cancelById(FolderBackupScannerWorker.UID);
        cancelById(UploadFolderFileAutomaticallyWorker.UID);
    }

    public void restartFolderUploadWorker(NetworkType networkType) {
        cancelFolderWorker();

        Disposable disposable = startWorkerUntilStopped(UploadFolderFileAutomaticallyWorker.UID).subscribe(new Action() {
            @Override
            public void run() {
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

        String workerName = UploadFolderFileAutomaticallyWorker.class.getSimpleName();
        boolean isRunning = checkWorkerIsRunningById(UploadFolderFileAutomaticallyWorker.UID);
        if (isRunning) {
            SLogs.w(workerName + " is running");
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadFolderFileAutomaticallyWorker.class)
                .setConstraints(constraints)
                .setId(UploadFolderFileAutomaticallyWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.REPLACE, request);
    }

    //
    public void cancelFilesUploadWorker() {
        cancelById(FolderBackupScannerWorker.UID);
        cancelById(UploadFolderFileAutomaticallyWorker.UID);
    }

    ///////////////////upload file///////////////////
    public void startFileUploadWorker() {
        String workerName = UploadFileManuallyWorker.class.getSimpleName();

        boolean isRunning = checkWorkerIsRunningById(UploadFileManuallyWorker.UID);
        if (isRunning) {
            SLogs.w(workerName + " is running");
        }

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadFileManuallyWorker.class)
                .setId(UploadFileManuallyWorker.UID)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, request);
    }


    ///////////////////download///////////////////
    public void scheduleOneTimeFilesDownloadScanWorker() {
        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileScanWorker.class)
                .build();
        SupportWorkManager.getWorkManager().enqueue(request);
    }

    public void scheduleOneTimeFilesDownloadScanWorker(String[] direntIds) {
        Data data = new Data.Builder()
                .putStringArray(TransferWorker.DATA_DIRENT_LIST_KEY, direntIds)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileScanWorker.class)
                .setInputData(data)
                .build();
        SupportWorkManager.getWorkManager().enqueue(request);
    }

    public void scheduleOneTimeFilesDownloadScanWorker(String transferId) {
        Data data = new Data.Builder()
                .putString(DownloadFileScanWorker.DATA_TRANSFER_KEY, transferId)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileScanWorker.class)
                .setInputData(data)
                .build();
        SupportWorkManager.getWorkManager().enqueue(request);
    }


    public void startFileDownloadWorker() {
        String workerName = DownloadWorker.class.getSimpleName();
        boolean isRunning = checkWorkerIsRunningById(DownloadWorker.UID);
        if (isRunning) {
            SLogs.w(workerName + " is running");
            return;
        }

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadWorker.class)
                .setId(DownloadWorker.UID)
                .build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(workerName, ExistingWorkPolicy.KEEP, request);
    }

    public void startDownloadCheckerWorker(String filePath) {
        Data data = new Data.Builder()
                .putString(DownloadedFileCheckerWorker.FILE_CHANGE_KEY, filePath)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadedFileCheckerWorker.class)
                .addTag(TAG_TRANSFER)
                .setInputData(data)
                .build();

        SupportWorkManager.getWorkManager().enqueue(request);
    }


    public void cancelFilesDownloadJob() {
        cancelById(DownloadWorker.UID);
        cancelById(DownloadedFileCheckerWorker.UID);
        cancelById(DownloadFileScanWorker.UID);
    }


    public void cancelAllJobs() {
        for (Disposable disposable : disposableList) {
            if (!disposable.isDisposed()) {
                disposable.dispose();
            }
        }

        SupportWorkManager.getWorkManager().cancelAllWork();
    }


}
