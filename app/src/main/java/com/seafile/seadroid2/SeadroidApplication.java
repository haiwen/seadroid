package com.seafile.seadroid2;


import android.app.Application;
import android.content.Context;
import android.util.Log;

import androidx.annotation.BoolRes;
import androidx.annotation.IntegerRes;
import androidx.annotation.StringRes;
import androidx.biometric.BiometricManager;
import androidx.work.Configuration;
import androidx.work.WorkManager;

import com.jeremyliao.liveeventbus.LiveEventBus;
import com.seafile.seadroid2.framework.monitor.ActivityMonitor;
import com.seafile.seadroid2.framework.notification.base.NotificationUtils;
import com.seafile.seadroid2.framework.util.CrashHandler;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.provider.DocumentCache;
import com.seafile.seadroid2.ui.LockedActivity;
import com.seafile.seadroid2.ui.camera_upload.AlbumBackupAdapterBridge;

import io.reactivex.exceptions.UndeliverableException;
import io.reactivex.functions.Consumer;
import io.reactivex.plugins.RxJavaPlugins;


public class SeadroidApplication extends Application {
    private static Context context = null;
    private static boolean isLocked = false;

    @Override
    public void onCreate() {
        super.onCreate();

        context = this;

        lockIfNeeded();

        //init xlog in com.seafile.seadroid2.provider.SeafileProvider#onCreate()
//        SLogs.init();

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

        //register album backup sync receiver
        AlbumBackupAdapterBridge.registerSyncReceiver(getAppContext());

        //This feature can be extended
        registerActivityLifecycleCallbacks(new ActivityMonitor());

        RxJavaPlugins.setErrorHandler(new Consumer<Throwable>() {
            @Override
            public void accept(Throwable throwable) throws Exception {
                if (throwable instanceof UndeliverableException e) {
                    SafeLogs.e(e);
                }
            }
        });
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

    public static boolean isLocked() {
        return isLocked;
    }

    public static boolean canLock() {
        // This allows the lock to be removed by, e.g., removing all biometrics/passwords,
        // but that's already a privileged action anyway.
        // In the future, it may be worthwhile looking into clearing auth data if passwords/biometrics
        // change, but this is a good balance of security vs convenience.
        return BiometricManager.from(context).canAuthenticate(LockedActivity.BIOMETRIC_REQS)
                == BiometricManager.BIOMETRIC_SUCCESS;
    }

    public static void lockIfNeeded() {
        // Only lock if we're allowed to and the device supports it.
        if (Settings.BIOMETRIC_LOCK_SWITCH.queryValue() && canLock()) {
            isLocked = true;
        }
    }

    public static void unlock() {
        isLocked = false;
    }
}