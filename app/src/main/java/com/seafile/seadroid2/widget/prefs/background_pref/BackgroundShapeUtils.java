package com.seafile.seadroid2.widget.prefs.background_pref;

import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.LayerDrawable;

public class BackgroundShapeUtils {


    public static LayerDrawable genBackgroundDrawable(int radiusPosition, int backgroundColor, int backgroundRadius) {
        // background shape
        GradientDrawable backgroundShape = new GradientDrawable();
        backgroundShape.setShape(GradientDrawable.RECTANGLE);
        backgroundShape.setColor(backgroundColor);

        float[] radii = getRadii(radiusPosition, backgroundRadius);
        backgroundShape.setCornerRadii(radii);

        Drawable[] layers = {backgroundShape};
        return new LayerDrawable(layers);
    }

    private static float[] getRadii(int radiusPosition, int backgroundRadius) {
        if (backgroundRadius == 0) {
            return new float[]{0, 0, 0, 0, 0, 0, 0, 0};
        }

        float[] radii;
        if (radiusPosition == 0) {
            //none
            radii = new float[]{0, 0, 0, 0, 0, 0, 0, 0};
        } else if (radiusPosition == 1) {
            //all
            radii = new float[]{
                    backgroundRadius, backgroundRadius, // left-top
                    backgroundRadius, backgroundRadius, // right-top
                    backgroundRadius, backgroundRadius, // right-bottom
                    backgroundRadius, backgroundRadius  // left-bottom
            };
        } else if (radiusPosition == 2) {
            //top
            radii = new float[]{
                    backgroundRadius, backgroundRadius,
                    backgroundRadius, backgroundRadius,
                    0, 0, 0, 0};
        } else if (radiusPosition == 3) {
            //bottom
            radii = new float[]{
                    0, 0, 0, 0,
                    backgroundRadius, backgroundRadius,
                    backgroundRadius, backgroundRadius
            };
        } else {
            radii = new float[]{0, 0, 0, 0, 0, 0, 0, 0};
        }
        return radii;
    }
}
