package com.seafile.seadroid2.ui.camera_upload;

import static android.app.PendingIntent.FLAG_IMMUTABLE;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.AbstractThreadedSyncAdapter;
import android.content.ComponentName;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SyncResult;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.MediaStore;
import android.util.Log;

import androidx.core.app.NotificationCompat;

import com.google.common.base.Joiner;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.data.CameraSyncEvent;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.CustomNotificationBuilder;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.settings.SettingsActivity;
import com.seafile.seadroid2.util.CameraSyncStatus;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.sp.SettingsManager;
import com.seafile.seadroid2.worker.BackgroundJobManagerImpl;

import org.greenrobot.eventbus.EventBus;

import java.io.File;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Sync adapter for media upload.
 * <p/>
 * This class uploads images/videos from the gallery to the configured seafile account.
 * It is not called directly, but managed by the Android Sync Manager instead.
 */
public class AlbumBackupAdapter extends AbstractThreadedSyncAdapter {
    private static final String DEBUG_TAG = "CameraSyncAdapter";

    private ContentResolver contentResolver;

    private String targetRepoId;
    private String targetRepoName;
    private List<String> bucketList;

    private final String BASE_DIR = "My Photos";

    /**
     * Media files we have sent over to the TransferService. Thread-safe.
     */
    private List<Integer> tasksInProgress = new ArrayList<>();


    /**
     * Set up the sync adapter
     */
    public AlbumBackupAdapter(Context context) {
        /*
         * autoInitialize is set to false because we need to handle initialization
         * ourselves in performSync() (resetting the photo database).
         */
        super(context, false);

        contentResolver = context.getContentResolver();
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
        BackgroundJobManagerImpl.getInstance().cancelMediaSyncJob();
    }

    @Override
    public void onPerformSync(android.accounts.Account account,
                              Bundle extras, String authority,
                              ContentProviderClient provider,
                              SyncResult syncResult) {

        SLogs.d("onPerformSync");

        if (!SettingsManager.getInstance().checkCameraUploadNetworkAvailable()) {
            // Log.d(DEBUG_TAG, "Not syncing because of data plan restriction.");
            // treat dataPlan abort the same way as a network connection error
            syncResult.stats.numIoExceptions++;
            SeadroidApplication.getInstance().setScanUploadStatus(CameraSyncStatus.NETWORK_UNAVAILABLE);
            EventBus.getDefault().post(new CameraSyncEvent("noNetwork"));
            return;
        }

        Account seafileAccount = SupportAccountManager.getInstance().getSeafileAccount(account);

        /**
         * this should never occur, as camera upload is supposed to be disabled once the camera upload
         * account signs out.
         */
        if (!seafileAccount.hasValidToken()) {
            Log.d(DEBUG_TAG, "This account has no auth token. Disable camera upload.");
            syncResult.stats.numAuthExceptions++;

            // we're logged out on this account. disable camera upload.
            ContentResolver.cancelSync(account, CameraUploadManager.AUTHORITY);
            ContentResolver.setIsSyncable(account, CameraUploadManager.AUTHORITY, 0);
            return;
        }

        //
        BackgroundJobManagerImpl.getInstance().scheduleOneTimeMediaSyncJob();
    }
}
