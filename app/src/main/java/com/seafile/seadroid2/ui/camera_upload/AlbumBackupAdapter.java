package com.seafile.seadroid2.ui.camera_upload;

import android.content.AbstractThreadedSyncAdapter;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SyncResult;
import android.os.Bundle;
import android.util.Log;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;

/**
 * Sync adapter for media upload.
 * <p/>
 * This class uploads images/videos from the gallery to the configured seafile account.
 * It is not called directly, but managed by the Android Sync Manager instead.
 */
public class AlbumBackupAdapter extends AbstractThreadedSyncAdapter {
    private static final String DEBUG_TAG = "CameraSyncAdapter";

    /**
     * Set up the sync adapter
     */
    public AlbumBackupAdapter(Context context) {
        /*
         * autoInitialize is set to false because we need to handle initialization
         * ourselves in performSync() (resetting the photo database).
         */
        super(context, false);
    }


    @Override
    public void onSecurityException(android.accounts.Account account, Bundle extras, String authority, SyncResult syncResult) {
        super.onSecurityException(account, extras, authority, syncResult);
        Log.e(DEBUG_TAG, syncResult.toString());
    }

    @Override
    public boolean onUnsyncableAccount() {
        Log.e(DEBUG_TAG, "onUnsyncableAccount");
        return super.onUnsyncableAccount();
    }

    @Override
    public void onSyncCanceled(Thread thread) {
        super.onSyncCanceled(thread);
        Log.e(DEBUG_TAG, "onSyncCanceled ->" + thread.getName());
    }

    @Override
    public void onSyncCanceled() {
        super.onSyncCanceled();

        Log.e(DEBUG_TAG, "onSyncCanceled");
        BackgroundJobManagerImpl.getInstance().cancelMediaWorker();
    }

    @Override
    public void onPerformSync(android.accounts.Account account,
                              Bundle extras, String authority,
                              ContentProviderClient provider,
                              SyncResult syncResult) {

        SLogs.d("onPerformSync");

        Account seafileAccount = SupportAccountManager.getInstance().getSeafileAccount(account);

        /**
         * this should never occur, as camera upload is supposed to be disabled once the camera upload
         * account signs out.
         */
        if (!seafileAccount.hasValidToken()) {
            Log.d(DEBUG_TAG, "This account has no auth token. Disable camera upload.");
            syncResult.stats.numAuthExceptions++;

            // we're logged out on this account. disable camera upload.
            CameraUploadManager.getInstance().disableSpecialAccountCameraUpload(seafileAccount);
            return;
        }

        boolean isEnable = AlbumBackupManager.readBackupSwitch();
        if (!isEnable) {
            return;
        }

        //start
        boolean isForce = extras.getBoolean(ContentResolver.SYNC_EXTRAS_MANUAL);
        BackgroundJobManagerImpl.getInstance().scheduleMediaScanWorker(isForce);
    }
}
