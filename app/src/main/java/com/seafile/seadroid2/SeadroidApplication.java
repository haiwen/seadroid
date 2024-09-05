package com.seafile.seadroid2;

import android.app.Application;
import android.content.Context;

import androidx.appcompat.app.AppCompatDelegate;

import com.jeremyliao.liveeventbus.LiveEventBus;
import com.seafile.seadroid2.enums.NightMode;
import com.seafile.seadroid2.framework.datastore.sp.Sorts;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.gesturelock.AppLockManager;
import com.seafile.seadroid2.framework.monitor.ActivityMonitor;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.util.CrashHandler;
import com.seafile.seadroid2.preferences.Settings;


public class SeadroidApplication extends Application {
    private static Context context;
    private static SeadroidApplication instance;

    @Override
    public void onCreate() {
        super.onCreate();

        instance = this;

        //
        Sorts.init();

        //init slogs
        SLogs.init();

        //print current app env info
        SLogs.printAppEnvInfo();

        //
        Settings.initUserSettings();
        if (Settings.APP_NIGHT_MODE != null) {
            NightMode nightMode = Settings.APP_NIGHT_MODE.queryValue();
            AppCompatDelegate.setDefaultNightMode(nightMode.ordinal());
        } else {
            AppCompatDelegate.setDefaultNightMode(NightMode.FOLLOW_SYSTEM.ordinal());
        }


        // set gesture lock if available
        AppLockManager.getInstance().enableDefaultAppLockIfAvailable(this);

        LiveEventBus.config()
                .autoClear(true)
                .enableLogger(BuildConfig.DEBUG)
                .setContext(this)
                .lifecycleObserverAlwaysActive(true);

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
