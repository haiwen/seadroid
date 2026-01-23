package com.seafile.seadroid2.ui.media.image;

import android.content.Context;
import android.graphics.Color;

import androidx.core.content.ContextCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.view.photoview.ScrollDirection;
import com.seafile.seadroid2.view.photoview.ScrollStatus;

public class ImagePreviewUtils {

    private static int grey000 = 0;
    private static int grey100 = 0;
    private static int grey911 = 0;

    public static int getGrey000(Context context) {
        if (grey000 == 0) {
            grey000 = ContextCompat.getColor(context, R.color.white);
        }
        return grey000;
    }

    public static int getGrey100(Context context) {
        if (grey100 == 0) {
            grey100 = ContextCompat.getColor(context, R.color.material_grey_109);
        }
        return grey100;
    }

    public static int getGreyAlpha000(Context context, int alpha) {
        int red = Color.red(getGrey000(context));
        int green = Color.green(getGrey000(context));
        int blue = Color.blue(getGrey000(context));
        return Color.argb(alpha, red, green, blue);
    }

    public static int getGrey911(Context context) {
        if (grey911 == 0) {
            grey911 = ContextCompatKt.getColorCompat(context, R.color.material_grey_911);
        }
        return grey911;
    }

    public static int getGreyAlpha911(Context context, int alpha) {
        int red = Color.red(getGrey911(context));
        int green = Color.green(getGrey911(context));
        int blue = Color.blue(getGrey911(context));
        return Color.argb(alpha, red, green, blue);
    }

}
