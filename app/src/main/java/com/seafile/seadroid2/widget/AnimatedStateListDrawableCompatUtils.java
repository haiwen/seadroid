package com.seafile.seadroid2.widget;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.GradientDrawable;

import com.blankj.utilcode.util.ColorUtils;

import java.util.stream.IntStream;

public class AnimatedStateListDrawableCompatUtils {
    private static final int[] PRIMARY_COLOR_SET = IntStream.of(androidx.appcompat.R.attr.colorPrimary).toArray();
    private static final int[] CHECKED_STATE_SET = IntStream.of(android.R.attr.state_checked).toArray();
    private static final int[] STATE_EMPTY = IntStream.of().toArray();

    public static androidx.appcompat.graphics.drawable.AnimatedStateListDrawableCompat createDrawableCompat(Context context) {
        androidx.appcompat.graphics.drawable.AnimatedStateListDrawableCompat animatedStateListDrawableCompat = new androidx.appcompat.graphics.drawable.AnimatedStateListDrawableCompat();
        animatedStateListDrawableCompat.setEnterFadeDuration(200);
        animatedStateListDrawableCompat.setExitFadeDuration(200);

        GradientDrawable gradientDrawable = new GradientDrawable();

        TypedArray typedArray = context.getTheme().obtainStyledAttributes(PRIMARY_COLOR_SET);
        int color = typedArray.getColor(0, 0);
        typedArray.recycle();
        int aColor = ColorUtils.setAlphaComponent(color, 0.12f);
        gradientDrawable.setColor(aColor);

        animatedStateListDrawableCompat.addState(CHECKED_STATE_SET, gradientDrawable);
        animatedStateListDrawableCompat.addState(STATE_EMPTY, new ColorDrawable(Color.TRANSPARENT));

        return animatedStateListDrawableCompat;
    }
}
