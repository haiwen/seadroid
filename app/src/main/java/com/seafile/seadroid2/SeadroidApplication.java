package com.seafile.seadroid2;


import android.app.Application;
import android.content.Context;
import android.util.Log;

import androidx.annotation.BoolRes;
import androidx.annotation.IntegerRes;
import androidx.annotation.StringRes;

import com.jeremyliao.liveeventbus.LiveEventBus;
import com.seafile.seadroid2.framework.monitor.ActivityMonitor;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.util.CrashHandler;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.provider.DocumentCache;


public class SeadroidApplication extends Application {
    private static Context context = null;

    @Override
    public void onCreate() {
        super.onCreate();

        context = this;

        Log.e("SeadroidApplication", "onCreate()");

        //init slogs
        SLogs.init();

        //print current app env info
        SLogs.printAppEnvInfo();

        //
        Settings.initUserSettings();

        // set gesture lock if available
        //
//        AppLockManager.getInstance().enableDefaultAppLockIfAvailable(this);

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

    public static String getAppString(@StringRes int resId) {
        return getAppContext().getResources().getString(resId);
    }

    public static String getAppString(@StringRes int resId, Object... formatArgs) {
        return getAppContext().getResources().getString(resId, formatArgs);
    }

    public static Boolean getAppBoolean(@BoolRes int resId) {
        return getAppContext().getResources().getBoolean(resId);
    }

    public static Integer getAppInteger(@IntegerRes int resId) {
        return getAppContext().getResources().getInteger(resId);
    }

    private final DocumentCache mCache = new DocumentCache();

    public static DocumentCache getDocumentCache() {
        return getApplication().mCache;
    }

    private static SeadroidApplication getApplication() {
        return (SeadroidApplication) getAppContext();
    }
}