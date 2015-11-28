package com.seafile.seadroid2.ui;

import android.app.Activity;
import android.widget.Toast;

/**
 * Utilities for displaying toast notifications
 */
public class ToastUtils {

    /**
     * Show the given message in a {@link Toast} for a short period of time
     * <p>
     * This method may be called from any thread
     *
     * @param activity
     * @param message
     */
    public static void show(final Activity activity, final String message) {
        if (activity == null)
            return;
        Toast.makeText(activity, message, Toast.LENGTH_SHORT).show();
    }

    /**
     * Show the given message in a {@link Toast} for a long period of time
     * <p>
     * This method may be called from any thread
     *
     * @param activity
     * @param message
     */
    public static void showLong(final Activity activity, final String message) {
        if (activity == null)
            return;
        Toast.makeText(activity, message, Toast.LENGTH_LONG).show();
    }
    /**
     * Show the message with the given resource id in a {@link Toast} for a short period of time
     * <p>
     * This method may be called from any thread
     *
     * @param activity
     * @param resId
     */
    public static void show(final Activity activity, final int resId) {
        if (activity == null)
            return;

        show(activity, activity.getString(resId));
    }

    /**
     * Show the message with the given resource id in a {@link Toast} for a long period of time
     * <p>
     * This method may be called from any thread
     *
     * @param activity
     * @param resId
     */
    public static void showLong(final Activity activity, final int resId) {
        if (activity == null)
            return;

        showLong(activity, activity.getString(resId));
    }
}
