package com.seafile.seadroid2;

import android.app.Application;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;

import androidx.appcompat.app.AppCompatDelegate;
import androidx.lifecycle.ViewModelProvider;

import com.joanzapata.iconify.Iconify;
import com.joanzapata.iconify.fonts.MaterialCommunityModule;
import com.seafile.seadroid2.gesturelock.AppLockManager;
import com.seafile.seadroid2.monitor.ActivityMonitor;
import com.seafile.seadroid2.notification.NotificationUtils;
import com.seafile.seadroid2.ui.CustomNotificationBuilder;
import com.seafile.seadroid2.util.CrashHandler;
import com.seafile.seadroid2.worker.SupportWorkManager;

public class SeadroidApplication extends Application {
    private static Context context;
    private static SeadroidApplication instance;

    private int waitingNumber;
    private int totalNumber;
    private int scanUploadStatus;
    private int totalBackup;
    private int waitingBackup;

    @Override
    public void onCreate() {
        super.onCreate();

        instance = this;

//        seaDroidViewModel = new ViewModelProvider.AndroidViewModelFactory(this).create(SeadroidViewModel.class);

        //night mode: no
//        AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_NO);

        //iconify
        Iconify.with(new MaterialCommunityModule());

        App.init();

        SupportWorkManager.initWorkManager();

        // set gesture lock if available
        AppLockManager.getInstance().enableDefaultAppLockIfAvailable(this);


        //
        NotificationUtils.initNotificationChannels(this);

        CrashHandler crashHandler = CrashHandler.getInstance();
        crashHandler.init(this);

        //This feature can be extended
        registerActivityLifecycleCallbacks(new ActivityMonitor());
    }

    @Override
    protected void attachBaseContext(Context base) {
        super.attachBaseContext(base);
        SeadroidApplication.context = this;
    }

    public static Context getAppContext() {
        return SeadroidApplication.context;
    }

    public static SeadroidApplication getInstance() {
        return instance;
    }

    public void setCameraUploadNumber(int waitingNumber, int totalNumber) {
        this.waitingNumber = waitingNumber;
        this.totalNumber = totalNumber;
    }

    public int getWaitingNumber() {
        return waitingNumber;
    }

    public int getTotalNumber() {
        return totalNumber;
    }

    public void setScanUploadStatus(int scanUploadStatus) {
        this.scanUploadStatus = scanUploadStatus;
    }

    public int getScanUploadStatus() {
        return scanUploadStatus;
    }

    public int getTotalBackup() {
        return totalBackup;
    }

    public int getWaitingBackup() {
        return waitingBackup;
    }

    public void setFolderBackupNumber(int totalBackup, int waitingBackup) {
        this.totalBackup = totalBackup;
        this.waitingBackup = waitingBackup;
    }
}
