package com.seafile.seadroid2.framework.monitor;

import android.app.Activity;
import android.app.Application;
import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.ui.LockedActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;


/**
 * Activity Monitor
 */
public class ActivityMonitor implements Application.ActivityLifecycleCallbacks {

    @Override
    public void onActivityCreated(@NonNull Activity activity, @Nullable Bundle savedInstanceState) {
        PreloadWebView.getInstance().preload();

        // Prevent any normal activities from being entered while the app is locked.
        if (SeadroidApplication.isLocked() && !(activity instanceof LockedActivity)) {
            Intent intent = new Intent(activity, LockedActivity.class);
            // Allow returning back to this initial activity.
            intent.putExtra("TARGET_INTENT", activity.getIntent());
            // Prevent back button / returning some other way.
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);

            activity.startActivity(intent);
            activity.finish();
        }
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
