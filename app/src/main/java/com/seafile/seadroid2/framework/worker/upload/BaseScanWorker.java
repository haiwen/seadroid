package com.seafile.seadroid2.framework.worker.upload;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

public abstract class BaseScanWorker extends TransferWorker {
    public BaseScanWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    protected boolean checkNetworkTypeIfAllowStartUploadWorker() {
        boolean isAllowData;
        if (getDataSource() == FeatureDataSource.ALBUM_BACKUP) {
            isAllowData = AlbumBackupSharePreferenceHelper.readAllowDataPlanSwitch();
        } else if (getDataSource() == FeatureDataSource.FOLDER_BACKUP) {
            isAllowData = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        } else {
            isAllowData = false;
        }

        if (isAllowData) {
            return true;
        }

        return !NetworkUtils.isMobileData();
    }

    public abstract FeatureDataSource getDataSource();
}
