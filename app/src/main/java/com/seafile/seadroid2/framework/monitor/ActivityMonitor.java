package com.seafile.seadroid2.framework.monitor;

import android.app.Activity;
import android.app.Application;
import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.framework.util.AppLockManager;
import com.seafile.seadroid2.ui.LockedActivity;
import com.seafile.seadroid2.view.webview.PreloadWebView;


/**
 * Activity Monitor
 */
public class ActivityMonitor implements Application.ActivityLifecycleCallbacks {

    @Override
    public void onActivityCreated(@NonNull Activity activity, @Nullable Bundle savedInstanceState) {
        PreloadWebView.getInstance().preload();
        redirectToLockIfNeeded(activity);
    }

    @Override
    public void onActivityStarted(@NonNull Activity activity) {

    }

    @Override
    public void onActivityResumed(@NonNull Activity activity) {
        // Also check on resume — handles returning from background where no new
        // activity is created but AppLockManager.onAppForegrounded() re-locked.
        redirectToLockIfNeeded(activity);
        onActivityVisibleChanged(activity, true); // activity visible
    }

    /**
     * Redirect to LockedActivity when the app is locked.
     * LockedActivity itself is exempt to avoid infinite loops.
     */
    private void redirectToLockIfNeeded(Activity activity) {
        if (AppLockManager.isLocked() && !(activity instanceof LockedActivity)) {
            Intent lockIntent = new Intent(activity, LockedActivity.class);
            lockIntent.putExtra(LockedActivity.EXTRA_TARGET_INTENT, activity.getIntent());
            lockIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
            activity.startActivity(lockIntent);
            activity.finish();
        }
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
