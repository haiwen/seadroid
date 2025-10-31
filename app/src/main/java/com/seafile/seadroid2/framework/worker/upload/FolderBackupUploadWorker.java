package com.seafile.seadroid2.framework.worker.upload;

import android.app.ForegroundServiceStartNotAllowedException;
import android.content.Context;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.work.ForegroundInfo;
import androidx.work.WorkerParameters;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.notification.FolderBackupNotificationHelper;
import com.seafile.seadroid2.framework.notification.base.BaseTransferNotificationHelper;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.util.List;
import java.util.UUID;

public class FolderBackupUploadWorker extends BaseUploadWorker {
    private static final String TAG = "FolderBackupUploadWorker";
    public static final UUID UID = UUID.nameUUIDFromBytes(TAG.getBytes());

    private final FolderBackupNotificationHelper notificationManager;
    // @FIXME fix this
    private static volatile boolean isWorkerRunning = false;

    private static void setIsRunning(boolean running) {
        isWorkerRunning = running;
    }

    public static boolean isIsWorkerRunning() {
        return isWorkerRunning;
    }

    public FolderBackupUploadWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);

        notificationManager = new FolderBackupNotificationHelper(context);
    }

    @Override
    public FeatureDataSource getFeatureDataSource() {
        return FeatureDataSource.FOLDER_BACKUP;
    }

    @Override
    public BaseTransferNotificationHelper getNotification() {
        return notificationManager;
    }

    @NonNull
    @Override
    public Result doWork() {
        return start();
    }

    @Override
    public void onStopped() {
        super.onStopped();

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            SLogs.e("Folder backup stopped, reason：" + getStopReason());
        }
    }

    private void showNotification() {
        String title = getApplicationContext().getString(R.string.settings_folder_backup_info_title);
        String subTitle = getApplicationContext().getString(R.string.upload_started);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
                showForegroundAsync(foregroundInfo);
            } catch (ForegroundServiceStartNotAllowedException e) {
                SLogs.e(e.getMessage());
            }
        } else {
            ForegroundInfo foregroundInfo = notificationManager.getForegroundNotification();
            showForegroundAsync(foregroundInfo);
        }
    }

    private static boolean canExc() {
        boolean isRunning = BackupThreadExecutor.getInstance().isFolderBackupRunning();
        return !isRunning;
    }

    private Result start() {
        SLogs.d(TAG, "start()", "started execution");


        if (!canExc()) {
            SLogs.e(TAG, "The folder scan task was not started, because the folder backup task is running");
            return Result.success();
        }

        setIsRunning(true);

        //send a start event
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_START);

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return returnSuccess();
        }


        boolean isTurnOn = FolderBackupSharePreferenceHelper.readBackupSwitch();
        if (!isTurnOn) {
            SafeLogs.d(TAG, "backup switch is off");
            return returnSuccess();
        }

        List<String> backupPaths = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(backupPaths)) {
            SafeLogs.d(TAG, "backup paths is empty");
            return returnSuccess();
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig == null) {
            SafeLogs.d(TAG, "repo config is null");
            return returnSuccess();
        }


        if (!NetworkUtils.isConnected()) {
            SafeLogs.d(TAG, "network is not connected");
            return returnSuccess();
        }

        boolean isAllowDataPlan = FolderBackupSharePreferenceHelper.readDataPlanAllowed();
        if (!isAllowDataPlan) {
            if (NetworkUtils.isMobileData()) {
                SafeLogs.e(TAG, "data plan is not allowed", "current network type", NetworkUtils.getNetworkType().name());
                return returnSuccess();
            }

            SafeLogs.d(TAG, "data plan is not allowed", "current network type", NetworkUtils.getNetworkType().name());
        } else {
            SafeLogs.d(TAG, "data plan is allowed", "current network type", NetworkUtils.getNetworkType().name());
        }

        //
        int totalPendingCount = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.getPendingCount();
        if (totalPendingCount <= 0) {
            SLogs.d(TAG, "start()", "backup queue is empty");
            return returnSuccess();
        }

        //
        showNotification();
        //send a upload event
//        sendActionEvent(TransferDataSource.FOLDER_BACKUP, TransferEvent.EVENT_UPLOADING);

        SafeLogs.e(TAG, "pending count: " + totalPendingCount);

        SeafException resultException = SeafException.SUCCESS;
        SeafException interruptException = SeafException.SUCCESS;
        SeafException lastException = SeafException.SUCCESS;

        while (true) {
            TransferModel transferModel = GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.pick();
            if (transferModel == null) {
                break;
            }

            try {

                transfer(account, transferModel);

            } catch (SeafException seafException) {
                // In some cases, the transmission needs to be interrupted
                boolean isInterrupt = isInterrupt(seafException);
                if (isInterrupt) {
                    SafeLogs.e("An exception occurred and the transmission has been interrupted");
                    notifyError(seafException);

                    interruptException = seafException;
                    break;
                } else {
                    lastException = seafException;
                    SafeLogs.e("An exception occurred and the next transfer will continue");
                }
            }
        }


        if (interruptException != SeafException.SUCCESS) {
            resultException = interruptException;
            SafeLogs.d(TAG, "all completed", "error msg:[interruptException]: " + resultException.getMessage());
        } else if (totalPendingCount == 1 && lastException != SeafException.SUCCESS) {
            resultException = lastException;
            SafeLogs.d(TAG, "all completed", "error msg:[lastException]: " + resultException.getMessage());
        } else {
            SafeLogs.d(TAG, "all completed");
        }

        String errorMsg = null;
        if (resultException != SeafException.SUCCESS) {
            errorMsg = resultException.getMessage();
            Toasts.show(R.string.backup_failed);
        } else {
            Toasts.show(R.string.backup_completed);
        }

        //
        setIsRunning(false);
        sendCompleteEvent(FeatureDataSource.FOLDER_BACKUP, errorMsg, totalPendingCount);
        return Result.success();
    }

    protected Result returnSuccess() {
        setIsRunning(false);
        send(FeatureDataSource.FOLDER_BACKUP, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE);
        return Result.success();
    }
}
