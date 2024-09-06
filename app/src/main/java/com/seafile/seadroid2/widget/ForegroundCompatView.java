package com.seafile.seadroid2.widget;

import android.content.res.ColorStateList;
import android.graphics.PorterDuff;
import android.graphics.drawable.Drawable;

import androidx.annotation.Nullable;

public interface ForegroundCompatView {
    @Nullable
    Drawable getSupportForeground();

    void setSupportForeground(@Nullable Drawable var1);

    int getSupportForegroundGravity();

    void setSupportForegroundGravity(int var1);

    void setSupportForegroundTintList(@Nullable ColorStateList var1);

    @Nullable
    ColorStateList getSupportForegroundTintList();

    void setSupportForegroundTintMode(@Nullable PorterDuff.Mode var1);

    @Nullable
    PorterDuff.Mode getSupportForegroundTintMode();
}
