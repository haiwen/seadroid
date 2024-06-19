package com.seafile.seadroid2.framework.monitor;

import android.app.Activity;
import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.view.webview.PreloadWebView;


/**
 * Activity Monitor
 */
public class ActivityMonitor implements Application.ActivityLifecycleCallbacks {

    @Override
    public void onActivityCreated(@NonNull Activity activity, @Nullable Bundle savedInstanceState) {
        PreloadWebView.getInstance().preload();
    }

    @Override
    public void onActivityStarted(@NonNull Activity activity) {

    }

    @Override
    public void onActivityResumed(@NonNull Activity activity) {
        onActivityVisibleChanged(activity, true); // activity visible
    }

    @Override
    public void onActivityPaused(@NonNull Activity activity) {
        onActivityVisibleChanged(activity, false);

    }

    @Override
    public void onActivityStopped(@NonNull Activity activity) {

    }

    @Override
    public void onActivitySaveInstanceState(@NonNull Activity activity, @NonNull Bundle outState) {

    }

    @Override
    public void onActivityDestroyed(@NonNull Activity activity) {
        PreloadWebView.getInstance().preload();
    }

    private void onActivityVisibleChanged(Activity activity, boolean isVisible) {

    }
}
