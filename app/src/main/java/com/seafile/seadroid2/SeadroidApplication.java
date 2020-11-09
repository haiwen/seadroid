package com.seafile.seadroid2;

import android.annotation.TargetApi;
import android.app.Application;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;
import android.os.Build;

import com.joanzapata.iconify.Iconify;
import com.joanzapata.iconify.fonts.MaterialCommunityModule;
import com.seafile.seadroid2.gesturelock.AppLockManager;
import com.seafile.seadroid2.ui.CustomNotificationBuilder;

public class SeadroidApplication extends Application {
    private static Context context;
    private int waitingNumber;
    private int totalNumber;
    private int scanUploadStatus;
    private static SeadroidApplication instance;

    public void onCreate() {
        super.onCreate();
        Iconify.with(new MaterialCommunityModule());
        instance = this;
        // set gesture lock if available
        AppLockManager.getInstance().enableDefaultAppLockIfAvailable(this);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            initNotificationChannel();
        }
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


    private void initNotificationChannel() {
        String channelName = getString(R.string.channel_name_error);
        createNotificationChannel(CustomNotificationBuilder.CHANNEL_ID_ERROR, channelName, NotificationManager.IMPORTANCE_DEFAULT,false,true);

        channelName = getString(R.string.channel_name_upload);
        createNotificationChannel(CustomNotificationBuilder.CHANNEL_ID_UPLOAD, channelName, NotificationManager.IMPORTANCE_LOW,false,false);

        channelName = getString(R.string.channel_name_download);
        createNotificationChannel(CustomNotificationBuilder.CHANNEL_ID_DOWNLOAD, channelName, NotificationManager.IMPORTANCE_LOW,false,false);

    }

    @TargetApi(Build.VERSION_CODES.O)
    private void createNotificationChannel(String channelId, String channelName, int importance,boolean isVibrate, boolean hasSound ) {
        NotificationChannel channel = new NotificationChannel(channelId, channelName, importance);
        channel.setShowBadge(true);
        channel.enableVibration(isVibrate);
        channel.enableLights(true);
        if (!hasSound) {
            channel.setSound(null, null);
        }
        NotificationManager notificationManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
        notificationManager.createNotificationChannel(channel);
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

}
