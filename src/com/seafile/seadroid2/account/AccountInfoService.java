package com.seafile.seadroid2.account;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import android.util.Log;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SettingsManager;

/**
 * automatically update Account info, like space usage, total space size, from background.
 * Space usage changes with time, so always keep the service running.
 *
 * Created by Logan on 14/12/8.
 */
public class AccountInfoService extends Service {
    public static final String DEBUG_TAG = "AccountInfoService";

    private AccountManager accountMgr;

    @Override
    public void onCreate() {
        Log.d(DEBUG_TAG, "onCreate");
        super.onCreate();
        accountMgr = new AccountManager(this);
    }

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        super.onDestroy();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");
        final Account act = accountMgr.getCurrentAccount();
        ConcurrentAsyncTask.execute(new Runnable() {
            @Override
            public void run() {
                accountMgr.getAccountInfoFromServer(act);
            }
        });
        return START_STICKY;
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

}
