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
import com.seafile.seadroid2.ui.CustomNotificationBuilder;
import com.seafile.seadroid2.util.CrashHandler;

public class SeadroidApplication extends Application {
    private static Context context;
    private static SeadroidApplication instance;

    private int waitingNumber;
    private int totalNumber;
    private int scanUploadStatus;
    private int totalBackup;
    private int waitingBackup;


    //Global View Model
    private SeadroidViewModel seaDroidViewModel;

    public SeadroidViewModel getSeaDroidViewModel() {
        return seaDroidViewModel;
    }

    @Override
    public void onCreate() {
        super.onCreate();

        instance = this;

        seaDroidViewModel = new ViewModelProvider.AndroidViewModelFactory(this).create(SeadroidViewModel.class);

        //night mode: no
        AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_NO);


        //iconify
        Iconify.with(new MaterialCommunityModule());

//        initImageLoader(getApplicationContext());

        App.init();

        // set gesture lock if available
        AppLockManager.getInstance().enableDefaultAppLockIfAvailable(this);

        initNotificationChannel();
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

//    public static void initImageLoader(Context context) {
//
//        File cacheDir = StorageManager.getInstance().getThumbnailsDir();
//        // This configuration tuning is custom. You can tune every option, you may tune some of them,
//        // or you can create default configuration by
//        //  ImageLoaderConfiguration.createDefault(this);
//        // method.
//        ImageLoaderConfiguration config = new ImageLoaderConfiguration.Builder(context)
//                .diskCache(new UnlimitedDiscCache(cacheDir))
//                .threadPriority(Thread.NORM_PRIORITY - 2)
//                .denyCacheImageMultipleSizesInMemory()
//                .diskCacheFileNameGenerator(new Md5FileNameGenerator())
//                .diskCacheSize(50 * 1024 * 1024) // 50 Mb
//                .tasksProcessingOrder(QueueProcessingType.LIFO)
//                .imageDownloader(new AuthImageDownloader(context, 10000, 10000))
//                .writeDebugLogs() // Remove for release app
//                .build();
//        // Initialize ImageLoader with configuration.
//        ImageLoader.getInstance().init(config);
//    }

    private void initNotificationChannel() {
        String channelName = getString(R.string.channel_name_error);
        createNotificationChannel(CustomNotificationBuilder.CHANNEL_ID_ERROR, channelName, NotificationManager.IMPORTANCE_DEFAULT, false, true);

        channelName = getString(R.string.channel_name_upload);
        createNotificationChannel(CustomNotificationBuilder.CHANNEL_ID_UPLOAD, channelName, NotificationManager.IMPORTANCE_LOW, false, false);

        channelName = getString(R.string.channel_name_download);
        createNotificationChannel(CustomNotificationBuilder.CHANNEL_ID_DOWNLOAD, channelName, NotificationManager.IMPORTANCE_LOW, false, false);

    }

    private void createNotificationChannel(String channelId, String channelName, int importance, boolean isVibrate, boolean hasSound) {
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
