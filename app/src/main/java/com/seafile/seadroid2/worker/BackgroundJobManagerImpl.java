package com.seafile.seadroid2.worker;

import android.annotation.SuppressLint;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.lifecycle.LiveData;
import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.ExistingWorkPolicy;
import androidx.work.ListenableWorker;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.util.sp.SettingsManager;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import kotlin.Pair;

public class BackgroundJobManagerImpl {
    public static final String TAG_ALL = "*";
    public static final String TAG_DOWNLOAD = TAG_ALL + ":DOWNLOAD";
    public static final String TAG_UPLOAD = TAG_ALL + ":UPLOAD";

    //download
    public static final String TAG_DOWNLOAD_ONETIME_FILES_SYNC = TAG_DOWNLOAD + ":worker_onetime_files_download_sync";
    public static final String TAG_DOWNLOAD_WORKER_FILES_DOWNLOAD = TAG_DOWNLOAD + ":worker_files_download";

    //upload
    public static final String TAG_UPLOAD_ONETIME_FILES_SYNC = TAG_UPLOAD + ":worker_onetime_files_upload_sync";
    public static final String TAG_UPLOAD_WORKER_FILES_UPLOAD = TAG_UPLOAD + ":worker_files_upload";

    //media
    public static final String TAG_UPLOAD_ONETIME_MEDIA_SYNC = TAG_UPLOAD + ":worker_onetime_media_upload_sync";
    public static final String TAG_UPLOAD_WORKER_MEDIA_UPLOAD = TAG_UPLOAD + ":worker_medias_upload";


    public static final String JOB_CONTENT_OBSERVER = "content_observer";
    public static final String JOB_PERIODIC_MEDIA_DETECTION = "periodic_media_detection";

    public static final String JOB_NOTIFICATION = "notification";

    public static final String JOB_PERIODIC_HEALTH_STATUS = "periodic_health_status";


    private final String TAG_PREFIX_NAME = "name";
    private final String TAG_PREFIX_USER = "user";
    private final String TAG_PREFIX_CLASS = "class";
    private final String TAG_PREFIX_START_TIMESTAMP = "timestamp";
    private final String NOT_SET_VALUE = "not_set";
    private final List<String> TAG_PREFIX_LIST = CollectionUtils.newArrayList(TAG_PREFIX_NAME, TAG_PREFIX_USER, TAG_PREFIX_CLASS, TAG_PREFIX_START_TIMESTAMP);


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


    private <T extends ListenableWorker> String formatClassTag(Class<T> t) {
        return TAG_PREFIX_CLASS + ":" + t.getSimpleName();
    }

    private String formatAccountTag() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account != null) {
            return TAG_PREFIX_USER + ":" + account.getSignature();
        }
        return "";
    }

    private String formatTimeStampTag(long t) {
        return TAG_PREFIX_START_TIMESTAMP + ":" + t;
    }

    private <T extends ListenableWorker> OneTimeWorkRequest.Builder oneTimeRequestBuilder(Class<T> tClass, String jobName) {
        return new OneTimeWorkRequest.Builder(tClass)
                .addTag(TAG_ALL)
                .addTag(jobName)
                .addTag(formatClassTag(tClass))
                .addTag(formatTimeStampTag(System.currentTimeMillis()))
                .addTag(formatAccountTag());
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
                .addTag(jobName)
                .addTag(formatClassTag(tClass))
                .addTag(formatTimeStampTag(System.currentTimeMillis()))
                .addTag(formatAccountTag());
    }

    public LiveData<List<WorkInfo>> getWorkInfosByTagLiveData(@NonNull String tag) {
        return SupportWorkManager.getWorkManager().getWorkInfosByTagLiveData(tag);
    }

    public List<JobInfo> getAllJobInfos() {
        LiveData<List<WorkInfo>> workInfosByTagLiveData = SupportWorkManager.getWorkManager().getWorkInfosByTagLiveData(TAG_ALL);
        if (workInfosByTagLiveData.getValue() == null) {
            return null;
        }

        List<WorkInfo> list = workInfosByTagLiveData.getValue();
        return list.stream().map(new Function<WorkInfo, JobInfo>() {
            @Override
            public JobInfo apply(WorkInfo workInfo) {
                return getJobInfo(workInfo);
            }
        }).sorted(new Comparator<JobInfo>() {
            @Override
            public int compare(JobInfo o1, JobInfo o2) {
                return o1.startTime < o2.startTime ? -1 : 1;
            }
        }).collect(Collectors.toList());
    }

    private JobInfo getJobInfoById(UUID uuid) {
        LiveData<WorkInfo> workInfoLiveData = SupportWorkManager.getWorkManager().getWorkInfoByIdLiveData(uuid);
        if (workInfoLiveData.getValue() == null) {
            return null;
        }

        WorkInfo workInfo = workInfoLiveData.getValue();
        return getJobInfo(workInfo);
    }

    private JobInfo getJobInfo(WorkInfo workInfo) {

        Set<String> tags = workInfo.getTags();
        HashMap<String, Object> hashMap = new HashMap<>();
        for (String tag : tags) {
            Pair<String, Object> pair = parseTag(tag);
            if (pair != null) {
                hashMap.put(pair.getFirst(), pair.getSecond());
            }
        }

        JobInfo j = new JobInfo();
        j.id = workInfo.getId();
        j.state = workInfo.getState().toString();
        j.workClass = hashMap.get(TAG_PREFIX_CLASS).toString();
        j.name = hashMap.get(TAG_PREFIX_NAME).toString();
        j.user = hashMap.get(TAG_PREFIX_USER).toString();
        j.startTime = Long.parseLong(hashMap.get(TAG_PREFIX_START_TIMESTAMP).toString());
        j.progress = workInfo.getProgress().getInt("progress", -1);

        return j;
    }

    private Pair<String, Object> parseTag(String tag) {
        if (TextUtils.isEmpty(tag)) {
            return null;
        }
        String[] split = tag.split(":");
        String k = split[0];
        if (TAG_PREFIX_LIST.contains(k)) {
            String v = split[1];
            return new Pair<>(k, v);
        }
        return null;
    }

//    public void scheduleMediaDetectionJob() {
//        Constraints constraints = new Constraints.Builder()
//                .addContentUriTrigger(MediaStore.Images.Media.INTERNAL_CONTENT_URI, true)
//                .addContentUriTrigger(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, true)
//                .addContentUriTrigger(MediaStore.Video.Media.INTERNAL_CONTENT_URI, true)
//                .addContentUriTrigger(MediaStore.Video.Media.EXTERNAL_CONTENT_URI, true)
//                .setTriggerContentMaxDelay(MAX_CONTENT_TRIGGER_DELAY_MS, TimeUnit.MILLISECONDS)
//                .build();
//        PeriodicWorkRequest request = periodicRequestBuilder(MediaDetectionWorker.class, JOB_PERIODIC_MEDIA_DETECTION, 0, 0)
//                .setConstraints(constraints)
//                .build();
//        SupportWorkManager.getWorkManager().enqueueUniquePeriodicWork(JOB_CONTENT_OBSERVER, ExistingPeriodicWorkPolicy.REPLACE, request);
//    }


    //media
    public void scheduleOneTimeMediaSyncJob() {

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadMediaSyncWorker.class, TAG_UPLOAD_ONETIME_MEDIA_SYNC)
                .addTag(TAG_UPLOAD)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_UPLOAD_ONETIME_MEDIA_SYNC, ExistingWorkPolicy.KEEP, request);
    }

    public void startAlbumBackupJob() {
        NetworkType networkType = NetworkType.UNMETERED;
        if (SettingsManager.getInstance().isDataPlanAllowed()) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadMediaWorker.class, TAG_UPLOAD_WORKER_MEDIA_UPLOAD)
                .addTag(TAG_UPLOAD)
                .setConstraints(constraints)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_UPLOAD_WORKER_MEDIA_UPLOAD, ExistingWorkPolicy.REPLACE, request);
    }

    public void startAccountRemovalJob() {
        OneTimeWorkRequest request = oneTimeRequestBuilder(AccountRemovalWorker.class, JOB_NOTIFICATION).build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(JOB_NOTIFICATION, ExistingWorkPolicy.KEEP, request);
    }


    //upload
    public void scheduleOneTimeFilesUploadSyncJob() {
        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadFileSyncWorker.class, TAG_UPLOAD_ONETIME_FILES_SYNC)
                .addTag(TAG_UPLOAD)
                .build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_UPLOAD_ONETIME_FILES_SYNC, ExistingWorkPolicy.KEEP, request);
    }

    public void startFilesUploadJob() {
        NetworkType networkType = NetworkType.UNMETERED;
        if (SettingsManager.getInstance().isFolderBackupDataPlanAllowed()) {
            networkType = NetworkType.CONNECTED;
        }

        Constraints constraints = new Constraints.Builder()
                .setRequiredNetworkType(networkType)
                .setRequiresBatteryNotLow(false)
                .setRequiresCharging(false)
                .setRequiresDeviceIdle(false)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(UploadFileWorker.class, TAG_UPLOAD_WORKER_FILES_UPLOAD)
                .addTag(TAG_UPLOAD)
                .setConstraints(constraints)
                .build();

        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_UPLOAD_WORKER_FILES_UPLOAD, ExistingWorkPolicy.REPLACE, request);
    }



    //download
    public void scheduleOneTimeFilesDownloadSyncJob(DirentModel direntModel) {
        @SuppressLint("RestrictedApi")
        Data data = new Data.Builder()
                .putString(TransferWorker.DATA_DIRENT_KEY, GsonUtils.toJson(direntModel))
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileSyncWorker.class, TAG_DOWNLOAD_ONETIME_FILES_SYNC)
                .addTag(TAG_DOWNLOAD)
                .setInputData(data)
                .build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_DOWNLOAD_ONETIME_FILES_SYNC, ExistingWorkPolicy.KEEP, request);
    }

    public void scheduleOneTimeFilesDownloadSyncJob() {
        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileSyncWorker.class, TAG_DOWNLOAD_ONETIME_FILES_SYNC)
                .addTag(TAG_DOWNLOAD)
                .build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_DOWNLOAD_ONETIME_FILES_SYNC, ExistingWorkPolicy.KEEP, request);
    }

    public void scheduleOneTimeFilesDownloadSyncJob(String transferId) {
        @SuppressLint("RestrictedApi")
        Data data = new Data.Builder()
                .putString(DownloadFileSyncWorker.DATA_TRANSFER_KEY, transferId)
                .build();

        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadFileSyncWorker.class, TAG_DOWNLOAD_ONETIME_FILES_SYNC)
                .addTag(TAG_DOWNLOAD)
                .setInputData(data)
                .build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_DOWNLOAD_ONETIME_FILES_SYNC, ExistingWorkPolicy.KEEP, request);
    }

    protected void startFileDownloadJob() {
        OneTimeWorkRequest request = oneTimeRequestBuilder(DownloadWorker.class, TAG_DOWNLOAD_WORKER_FILES_DOWNLOAD)
                .addTag(TAG_DOWNLOAD)
                .build();
        SupportWorkManager.getWorkManager().enqueueUniqueWork(TAG_DOWNLOAD_WORKER_FILES_DOWNLOAD, ExistingWorkPolicy.KEEP, request);
    }

    public void cancelFilesUploadJob() {
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_UPLOAD_WORKER_FILES_UPLOAD);
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_UPLOAD_ONETIME_FILES_SYNC);
    }

    public void cancelFilesDownloadJob() {
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_DOWNLOAD_ONETIME_FILES_SYNC);
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_DOWNLOAD_WORKER_FILES_DOWNLOAD);
    }

    public void cancelMediaSyncJob() {
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_UPLOAD_ONETIME_MEDIA_SYNC);
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_UPLOAD_WORKER_MEDIA_UPLOAD);
    }

    public void startHealthStatus() {

    }

    public void cancelAllJobs() {
        SupportWorkManager.getWorkManager().cancelAllWorkByTag(TAG_ALL);
    }


}
