package com.seafile.seadroid2;

import java.io.File;

import android.app.Application;
import android.content.Context;

import com.nostra13.universalimageloader.cache.disc.impl.UnlimitedDiscCache;
import com.nostra13.universalimageloader.cache.disc.naming.Md5FileNameGenerator;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.ImageLoaderConfiguration;
import com.nostra13.universalimageloader.core.assist.QueueProcessingType;
import com.seafile.seadroid2.avatar.AuthImageDownloader;
import com.seafile.seadroid2.data.DataManager;

public class SeadroidApplication extends Application {
    private static Context context;
    
    public void onCreate() {
        super.onCreate();
        SeadroidApplication.context = getApplicationContext();
        initImageLoader(getApplicationContext());
    }
    
    public static Context getAppContext() {
        return SeadroidApplication.context;
    }
    
    public static void initImageLoader(Context context) {
        
        File cacheDir = DataManager.getAvatarCacheDirectory();
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
}
