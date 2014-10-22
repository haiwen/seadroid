package com.seafile.seadroid2.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.app.Service;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.IBinder;
import android.util.Log;

import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;

/**
 *  manage loading, caching, updating Avatar operations from background   
 *
 */
public class AvatarManageService extends Service {
    private static final String DEBUG_TAG = "AvatarManageService";

    private AvatarManager avatarManager;
    private ArrayList<Account> accounts;
    private AccountManager accountManager;
    
    @Override
    public void onCreate() {
        Log.d(DEBUG_TAG, "onCreate");
    }

    @Override
    public void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");
        accountManager = new AccountManager(getApplicationContext());
        accounts = (ArrayList<Account>) accountManager.getAccountList();
        ConcurrentAsyncTask.execute(new AvatarLoadTask(accounts));
        return START_STICKY;
    }
    
    private class AvatarLoadTask extends AsyncTask<Void, Void, Void>{
        public AvatarLoadTask(List<Account> accounts) {
            avatarManager = new AvatarManager(accounts);
        }
        @Override
        protected Void doInBackground(Void... params) {
            try {
                avatarManager.getAvatars(48);
            } catch (SeafException e) {
                e.printStackTrace();
            }
            return null;
        }
        
    }
    
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}
