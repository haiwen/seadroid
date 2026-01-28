package com.seafile.seadroid2.framework.service.upload;

import android.content.Context;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.ITransferNotification;
import com.seafile.seadroid2.framework.service.ParentEventTransfer;
import com.seafile.seadroid2.framework.service.scan.FolderScanHelper;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class FolderBackupScanner extends ParentEventTransfer {
    private final String TAG = "Folder-Backup-Thread-Scanner";

    public FolderBackupScanner(Context context, ITransferNotification iTransferNotificationDispatcher) {
        super(context, iTransferNotificationDispatcher);
    }


    protected SeafException returnSuccess() {
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCAN_COMPLETE);
        return SeafException.SUCCESS;
    }

    public SeafException scan(boolean isForce) {
        SafeLogs.e(TAG, "文件夹扫描器启动");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }

        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the switch is off");
            return returnSuccess();
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null || StringUtils.isEmpty(repoConfig.getRepoId())) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the repo is not selected");
            return returnSuccess();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SafeLogs.d(TAG, "The folder scan task was not started, because the folder path is not selected");
            return returnSuccess();
        }

        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.e(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type: ", NetworkUtils.getNetworkType().name());
        }

        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        //send a scan event
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_SCANNING);

        //scan
        long lastScanTime = 0L;// scan all files if lastScanTime is 0
        SeafException seafException = FolderScanHelper.traverseBackupPath(backupPaths, account, repoConfig, lastScanTime);

        if (seafException != SeafException.SUCCESS) {
            SafeLogs.e(TAG, "scan failed");
            return returnSuccess();
        }

        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        String content = null;
        if (totalPendingCount > 0) {
            boolean isAllowUpload = FolderScanHelper.checkNetworkTypeIfAllowStartUpload();
            if (!isAllowUpload) {
                content = TransferResult.WAITING.name();
            }
        }

        sendCompleteEvent(FeatureDataSource.FOLDER_BACKUP, content, totalPendingCount);
        return SeafException.SUCCESS;
    }
}
