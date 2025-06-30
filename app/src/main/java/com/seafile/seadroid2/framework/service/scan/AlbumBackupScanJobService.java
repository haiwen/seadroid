package com.seafile.seadroid2.framework.service.scan;

import android.app.job.JobParameters;
import android.app.job.JobService;

import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;

@Deprecated
public class AlbumBackupScanJobService extends JobService {
    @Override
    public boolean onStartJob(JobParameters params) {
        return false;
    }

    @Override
    public boolean onStopJob(JobParameters params) {
        return false;
    }
}
