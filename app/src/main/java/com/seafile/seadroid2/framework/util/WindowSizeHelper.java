package com.seafile.seadroid2.framework.util;

import android.app.Activity;

import androidx.annotation.NonNull;
import androidx.window.layout.WindowMetrics;
import androidx.window.layout.WindowMetricsCalculator;

/** Calculates responsive layout classes from the current application window. */
public final class WindowSizeHelper {
    public static final int BREAKPOINT_MEDIUM = 600;
    public static final int BREAKPOINT_EXPANDED = 840;

    public enum SizeClass {
        COMPACT,
        MEDIUM,
        EXPANDED
    }

    private WindowSizeHelper() {
    }

    @NonNull
    public static SizeClass getWidthSizeClass(@NonNull Activity activity) {
        int widthDp = getWindowWidthDp(activity);
        if (widthDp >= BREAKPOINT_EXPANDED) {
            return SizeClass.EXPANDED;
        }
        if (widthDp >= BREAKPOINT_MEDIUM) {
            return SizeClass.MEDIUM;
        }
        return SizeClass.COMPACT;
    }

    public static int getWindowWidthDp(@NonNull Activity activity) {
        return toDp(activity, getCurrentBounds(activity).width());
    }

    public static int getWindowHeightDp(@NonNull Activity activity) {
        return toDp(activity, getCurrentBounds(activity).height());
    }

    private static int toDp(@NonNull Activity activity, int pixels) {
        float density = activity.getResources().getDisplayMetrics().density;
        return Math.round(pixels / density);
    }

    @NonNull
    private static android.graphics.Rect getCurrentBounds(@NonNull Activity activity) {
        WindowMetrics metrics = WindowMetricsCalculator.getOrCreate()
                .computeCurrentWindowMetrics(activity);
        return metrics.getBounds();
    }
}
