package com.seafile.seadroid2.ui.camera_upload;

import android.content.AbstractThreadedSyncAdapter;
import android.content.ContentProviderClient;
import android.content.Context;
import android.content.SyncResult;
import android.os.Bundle;
import android.text.TextUtils;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.util.SLogs;

/**
 * The system triggers a compatibility entry point for album backup, but is not responsible for the lifecycle management of upload tasks.
 */
public class AlbumBackupAdapter extends AbstractThreadedSyncAdapter {
    private final String TAG = "AlbumBackupAdapter";

    public AlbumBackupAdapter(Context context) {
        super(context, false);
    }

    @Override
    public void onPerformSync(android.accounts.Account androidAccount,
                              Bundle extras, String authority,
                              ContentProviderClient provider,
                              SyncResult syncResult) {
        Account account = SupportAccountManager.getInstance().getSeafileAccount(androidAccount);
        if (!account.hasValidToken()) {
            syncResult.stats.numAuthExceptions++;
            return;
        }

        CameraUploadManager manager = CameraUploadManager.getInstance();
        Account cameraAccount = manager.getCameraAccount();
        if (cameraAccount == null || !TextUtils.equals(cameraAccount.getSignature(), account.getSignature())) {
            return;
        }

        // 异步交接后由共享执行器管理任务，系统取消回调不能停止应用内发起的备份。
        // After asynchronous handover, the shared executor manages the tasks, and the system's cancellation of callbacks cannot stop the backup initiated within the application.
        SLogs.d(TAG, "The system synchronously triggers album backups, and the upload lifecycle is managed by the shared executor.");
        manager.performSync(extras.getBoolean(CameraUploadManager.KEY_IS_FULL_SCAN, false));
    }
}
