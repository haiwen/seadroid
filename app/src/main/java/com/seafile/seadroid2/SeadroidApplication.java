package com.seafile.seadroid2;

import android.annotation.TargetApi;
import android.app.Application;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;
import android.os.Build;

import com.joanzapata.iconify.Iconify;
import com.joanzapata.iconify.fonts.MaterialCommunityModule;
import com.nostra13.universalimageloader.cache.disc.impl.UnlimitedDiscCache;
import com.nostra13.universalimageloader.cache.disc.naming.Md5FileNameGenerator;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.ImageLoaderConfiguration;
import com.nostra13.universalimageloader.core.assist.QueueProcessingType;
import com.seafile.seadroid2.avatar.AuthImageDownloader;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.gesturelock.AppLockManager;
import com.seafile.seadroid2.ui.CustomNotificationBuilder;

import java.io.File;

public class SeadroidApplication extends Application {
    private static Context context;
    
    public void onCreate() {
        super.onCreate();
        Iconify.with(new MaterialCommunityModule());

        initImageLoader(getApplicationContext());

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

    public static void initImageLoader(Context context) {
        
        File cacheDir = StorageManager.getInstance().getThumbnailsDir();
        // This configuration tuning is custom. You can tune every option, you may tune some of them,
        // or you can create default configuration by
        //  ImageLoaderConfiguration.createDefault(this);
        // method.
        ImageLoaderConfiguration config = new ImageLoaderConfiguration.Builder(context)
                .diskCache(new UnlimitedDiscCache(cacheDir))
                .threadPriority(Thread.NORM_PRIORITY - 2)
                .denyCacheImageMultipleSizesInMemory()
                .diskCacheFileNameGenerator(new Md5FileNameGenerator())
                .diskCacheSize(50 * 1024 * 1024) // 50 Mb
                .tasksProcessingOrder(QueueProcessingType.LIFO)
                .imageDownloader(new AuthImageDownloader(context, 10000, 10000))
                .writeDebugLogs() // Remove for release app
                .build();
        // Initialize ImageLoader with configuration.
        ImageLoader.getInstance().init(config);
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
}
