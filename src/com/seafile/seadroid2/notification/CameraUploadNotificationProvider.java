package com.seafile.seadroid2.notification;

import android.app.PendingIntent;
import android.content.Intent;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.transfer.*;
import com.seafile.seadroid2.ui.activity.TransferActivity;

import java.util.List;

/**
 * Camera upload notification provider
 */
public class CameraUploadNotificationProvider extends BaseNotificationProvider {

    public CameraUploadNotificationProvider(TransferManager transferManager, TransferService transferService) {
        super(transferManager, transferService);
    }

    @Override
    protected NotificationState getState() {
        if (txService == null)
            return NotificationState.NOTIFICATION_STATE_COMPLETED;

        List<UploadTaskInfo> infos = txService.getAllUploadTaskInfos();

        int progressCount = 0;
        int errorCount = 0;

        for (UploadTaskInfo info : infos) {
            if (info == null
                    || info.isCopyToLocal)
                continue;

            if (info.state.equals(TaskState.INIT)
                    || info.state.equals(TaskState.TRANSFERRING))
                progressCount++;
            else if (info.state.equals(TaskState.FAILED)
                    || info.state.equals(TaskState.CANCELLED))
                errorCount++;
        }

        if (progressCount == 0 && errorCount == 0)
            return NotificationState.NOTIFICATION_STATE_COMPLETED;
        else if (progressCount == 0 && errorCount > 0)
            return NotificationState.NOTIFICATION_STATE_COMPLETED_WITH_ERRORS;
        else // progressCount > 0
            return NotificationState.NOTIFICATION_STATE_PROGRESS;
    }

    @Override
    protected int getNotificationID() {
        return NOTIFICATION_ID_CAMERA_UPLOAD;
    }

    @Override
    protected String getNotificationTitle() {
        return SeadroidApplication.getAppContext().getString(R.string.notification_camera_upload_title);
    }

    @Override
    protected void notifyStarted() {
        Intent uIntent = new Intent(SeadroidApplication.getAppContext(), TransferActivity.class);
        uIntent.putExtra(NOTIFICATION_MESSAGE_KEY, NOTIFICATION_OPEN_UPLOAD_TAB);
        uIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);

        PendingIntent uPendingIntent = PendingIntent.getActivity(SeadroidApplication.getAppContext(),
                (int) System.currentTimeMillis(),
                uIntent,
                0);
        mNotifBuilder = CustomNotificationBuilder.getNotificationBuilder(SeadroidApplication.getAppContext())
                .setSmallIcon(R.drawable.icon)
                .setContentTitle(SeadroidApplication.getAppContext().getString(R.string.notification_camera_upload_title))
                .setOngoing(true)
                .setContentText(SeadroidApplication.getAppContext().getString(R.string.notification_camera_upload_title))
                .setContentIntent(uPendingIntent)
                .setProgress(100, 0, false);

        // Make this service run in the foreground, supplying the ongoing
        // notification to be shown to the user while in this state.
        txService.startForeground(NOTIFICATION_ID_CAMERA_UPLOAD, mNotifBuilder.build());
    }

    @Override
    protected String getProgressInfo() {
        String progressStatus = "";

        if (txService == null)
            return progressStatus;

        // failed or cancelled tasks won`t be shown in notification state
        // but failed or cancelled detailed info can be viewed in TransferList
        if (getState().equals(NotificationState.NOTIFICATION_STATE_COMPLETED_WITH_ERRORS))
            progressStatus = SeadroidApplication.getAppContext().getString(R.string.notification_camera_upload_completed);
        else if (getState().equals(NotificationState.NOTIFICATION_STATE_COMPLETED))
            progressStatus = SeadroidApplication.getAppContext().getString(R.string.notification_camera_upload_completed);
        else if (getState().equals(NotificationState.NOTIFICATION_STATE_PROGRESS)) {
            int uploadingCount = 0;
            List<UploadTaskInfo> infos = txService.getAllUploadTaskInfos();
            for (UploadTaskInfo info : infos) {
                if (info.isCopyToLocal)
                    continue;
                if (info.state.equals(TaskState.INIT)
                        || info.state.equals(TaskState.TRANSFERRING))
                    uploadingCount++;
            }

            if (uploadingCount != 0)
                progressStatus = SeadroidApplication.getAppContext().getResources().
                        getQuantityString(R.plurals.notification_upload_info,
                                uploadingCount,
                                uploadingCount,
                                getProgress());
        }
        return progressStatus;
    }

    @Override
    protected int getProgress() {
        long uploadedSize = 0l;
        long totalSize = 0l;
        if (txService == null)
            return 0;

        List<UploadTaskInfo> infos = txService.getAllUploadTaskInfos();
        for (UploadTaskInfo info : infos) {
            if (info == null
                    || info.isCopyToLocal)
                continue;
            uploadedSize += info.uploadedSize;
            totalSize += info.totalSize;
        }

        if (totalSize == 0)
            return 0;

        return (int) (uploadedSize * 100 / totalSize);
    }
}
