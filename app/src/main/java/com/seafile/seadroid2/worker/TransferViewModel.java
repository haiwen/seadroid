package com.seafile.seadroid2.worker;

import android.app.Application;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.ViewModel;
import androidx.work.ExistingWorkPolicy;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkInfo;
import androidx.work.WorkManager;

import java.util.List;
import java.util.concurrent.TimeUnit;

public class TransferViewModel extends ViewModel {

    private WorkManager workManager;
    private LiveData<List<WorkInfo>> workInfoLiveData;

    public TransferViewModel(Application application) {
        workManager = WorkManager.getInstance(application);
        workInfoLiveData = workManager.getWorkInfosByTagLiveData("");

        PeriodicWorkRequest workRequest = new PeriodicWorkRequest.Builder(TransferWorker.class, 1, TimeUnit.HOURS)
                .build();
        workRequest.getId();

        workManager.enqueue(workRequest);
    }
}
