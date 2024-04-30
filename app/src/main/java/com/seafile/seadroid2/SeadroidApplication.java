package com.seafile.seadroid2;

import android.app.Application;
import android.content.Context;

import com.seafile.seadroid2.gesturelock.AppLockManager;
import com.seafile.seadroid2.monitor.ActivityMonitor;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.util.CrashHandler;


public class SeadroidApplication extends Application {
    private static Context context;
    private static SeadroidApplication instance;

    @Override
    public void onCreate() {
        super.onCreate();

        instance = this;

        //
        App.init();

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
        context = this;
    }

    public static Context getAppContext() {
        return context;
    }

    public static SeadroidApplication getInstance() {
        return instance;
    }

}
