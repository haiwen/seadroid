package com.seafile.seadroid2.framework.util;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.PowerManager;
import android.provider.Settings;

import androidx.media3.common.util.Log;

import com.seafile.seadroid2.annotation.Unstable;

public class BatteryOptimizationUtil {
    private static final String TAG = "BatteryOptimization";

    /**
     * Checks if the app is currently ignoring battery optimizations (i.e., set to "Unrestricted").
     *
     * @param context The application context.
     * @return true if the app is ignoring battery optimizations, false otherwise.
     */
    public static boolean isIgnoringBatteryOptimizations(Context context) {
        String packageName = context.getPackageName();
        PowerManager powerManager = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
        if (powerManager != null) {
            return powerManager.isIgnoringBatteryOptimizations(packageName);
        } else {
            SLogs.e(TAG, "PowerManager is null, cannot check battery optimization status.");
            return false; // Fallback or handle error appropriately
        }
    }

    /**
     * Opens the battery optimization settings screen for the app.
     * This allows the user to manually change the setting.
     * <p>
     * Note: Directly adding the app to the whitelist programmatically (ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS)
     * is generally discouraged by Google Play unless the app's core functionality critically depends on it
     * and falls into an acceptable use case.
     *
     * @param context The application context.
     */
    @Unstable
    public static void requestIgnoreBatteryOptimizationsManually(Context context) {
        try {
            Intent intent = new Intent();
            String packageName = context.getPackageName();
            PowerManager pm = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
            // Check if already ignoring, though you might call this method regardless
            // if you want to simply show the user the settings page.
            // if (pm != null && !pm.isIgnoringBatteryOptimizations(packageName)) { // Optional check
            intent.setAction(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
            intent.setData(Uri.parse("package:" + packageName));
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK); // Necessary if called from non-Activity context
            if (intent.resolveActivity(context.getPackageManager()) != null) {
                context.startActivity(intent);
            } else {
                Log.e(TAG, "No activity found to handle ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS");
                // Fallback: Open general battery optimization settings or app info
                openGeneralBatterySettings(context);
            }
            // }
        } catch (Exception e) {
            Log.e(TAG, "Error opening battery optimization settings", e);
            // Fallback in case of any exception
            openGeneralBatterySettings(context);
        }
    }

    /**
     * Opens the general battery optimization settings screen, or a specific app's battery usage screen
     * as a fallback if ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS fails.
     *
     * @param context The application context
     */
    @Unstable
    public static void openGeneralBatterySettings(Context context) {
        try {
            Intent intent = new Intent();
            // Android 8.0+
            // This intent usually takes users to a list of apps where they can manage battery usage
            intent.setAction(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
            intent.putExtra(Settings.EXTRA_APP_PACKAGE, context.getPackageName());
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            if (intent.resolveActivity(context.getPackageManager()) != null) {
                context.startActivity(intent);
            } else {
                Log.e(TAG, "No activity found to handle general battery/app settings.");
            }
        } catch (Exception e) {
            Log.e(TAG, "Error opening general battery/app settings", e);
        }
    }
}
