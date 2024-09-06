package com.seafile.seadroid2.framework.util;

import android.content.res.ColorStateList;
import android.graphics.PorterDuff;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.widget.ForegroundCompatView;

public class ForegroundCompat {
    private ForegroundCompat() {
    }

    @Nullable
    public static Drawable getForeground(@NonNull View view) {
        if (view instanceof FrameLayout) {
            return ((FrameLayout) view).getForeground();
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            return view.getForeground();
        } else {
            return view instanceof ForegroundCompatView ? ((ForegroundCompatView) view).getSupportForeground() : null;
        }
    }

    public static void setForeground(@NonNull View view, @Nullable Drawable foreground) {
        if (view instanceof FrameLayout) {
            ((FrameLayout) view).setForeground(foreground);
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            view.setForeground(foreground);
        } else if (view instanceof ForegroundCompatView) {
            ((ForegroundCompatView) view).setSupportForeground(foreground);
        }

    }

    public static int getForegroundGravity(@NonNull View view) {
        if (view instanceof FrameLayout) {
            return ((FrameLayout) view).getForegroundGravity();
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            return view.getForegroundGravity();
        } else {
            return view instanceof ForegroundCompatView ? ((ForegroundCompatView) view).getSupportForegroundGravity() : 8388659;
        }
    }

    public static void setForegroundGravity(@NonNull View view, int gravity) {
        if (view instanceof FrameLayout) {
            ((FrameLayout) view).setForegroundGravity(gravity);
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            view.setForegroundGravity(gravity);
        } else if (view instanceof ForegroundCompatView) {
            ((ForegroundCompatView) view).setSupportForegroundGravity(gravity);
        }

    }

    public static void setForegroundTintList(@NonNull View view, @Nullable ColorStateList tint) {
        if (view instanceof FrameLayout) {
            ((FrameLayout) view).setForegroundTintList(tint);
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            view.setForegroundTintList(tint);
        } else if (view instanceof ForegroundCompatView) {
            ((ForegroundCompatView) view).setSupportForegroundTintList(tint);
        }

    }

    @Nullable
    public static ColorStateList getForegroundTintList(@NonNull View view) {
        if (view instanceof FrameLayout) {
            return ((FrameLayout) view).getForegroundTintList();
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            return view.getForegroundTintList();
        } else {
            return view instanceof ForegroundCompatView ? ((ForegroundCompatView) view).getSupportForegroundTintList() : null;
        }
    }

    public static void setForegroundTintMode(@NonNull View view, @Nullable PorterDuff.Mode tintMode) {
        if (view instanceof FrameLayout) {
            ((FrameLayout) view).setForegroundTintMode(tintMode);
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            view.setForegroundTintMode(tintMode);
        } else if (view instanceof ForegroundCompatView) {
            ((ForegroundCompatView) view).setSupportForegroundTintMode(tintMode);
        }

    }

    @Nullable
    public static PorterDuff.Mode getForegroundTintMode(@NonNull View view) {
        if (view instanceof FrameLayout) {
            return ((FrameLayout) view).getForegroundTintMode();
        } else if (Build.VERSION.SDK_INT >= 23 && isTargetingMOrAbove(view)) {
            return view.getForegroundTintMode();
        } else {
            return view instanceof ForegroundCompatView ? ((ForegroundCompatView) view).getSupportForegroundTintMode() : null;
        }
    }

    private static boolean isTargetingMOrAbove(@NonNull View view) {
        return view.getContext().getApplicationInfo().targetSdkVersion >= 23;
    }
}
