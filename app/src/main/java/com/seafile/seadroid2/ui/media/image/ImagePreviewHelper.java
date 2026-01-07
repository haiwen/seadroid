package com.seafile.seadroid2.ui.media.image;

import android.animation.ValueAnimator;
import android.content.Context;
import android.view.View;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.view.photoview.ScrollDirection;
import com.seafile.seadroid2.view.photoview.ScrollStatus;

public class ImagePreviewHelper {
    public static final String TAG = "ImagePreviewHelper";
    private Context context;
    private View[] views;
    private boolean isFullAlpha = true;

    public ImagePreviewHelper(Context context) {
        this.context = context;
    }

    public void setActionViews(View... vs) {
        if (vs == null || vs.length == 0) {
            return;
        }
        views = new View[vs.length];
        System.arraycopy(vs, 0, views, 0, vs.length);
    }

    private boolean canNot() {
        return views == null || views.length == 0;
    }


    private final float fraction = 0.02f;

    public void gradientLayout(DetailLayoutShowModel showModel) {

        if (canNot()) {
            return;
        }

        ScrollDirection direction = showModel.direction;
        int totalDistance = showModel.distance;
        ScrollStatus scrollStatus = showModel.status;
        boolean isDetailShowing = showModel.isShowing;

        SLogs.d(TAG, "direction = " + direction.name() + ", totalDistance = " + totalDistance + ", scrollStatus = " + scrollStatus + ", isDetailShowing = " + isDetailShowing);


        if (scrollStatus == ScrollStatus.CANCELLED) {
            animateView(!isDetailShowing);
            return;
        }

        if (scrollStatus == ScrollStatus.FINISHED) {
            fillViewAlpha(isDetailShowing ? 1f : 0f);
            return;
        }


        float a = getViewAlpha();
        if (direction == ScrollDirection.DOWN) {
            //0 - 1
            a += fraction;
        } else if (direction == ScrollDirection.UP) {
            //1 - 0
            a -= fraction;
        }

        if (a > 1) {
            a = 1;
        }
        if (a < 0) {
            a = 0;
        }

        fillViewAlpha(a);
    }

    private float getViewAlpha() {
        if (canNot()) {
            return 0f;
        }

        return views[0].getAlpha();
    }

    private void animateView(boolean isFullAlpha) {

        if (canNot()) {
            return;
        }

        float b = getViewAlpha();
        ValueAnimator animator = ValueAnimator.ofFloat(b, isFullAlpha ? 1f : 0f);
        animator.setDuration(200);
        animator.setInterpolator(new LinearInterpolator());
        animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
            @Override
            public void onAnimationUpdate(@NonNull ValueAnimator animation) {
                float a = (float) animation.getAnimatedValue();
                fillViewAlpha(a);
            }
        });
        animator.start();
    }

    public void fillViewAlpha(float alpha) {
        if (canNot()) {
            return;
        }

        for (View view : views) {
            if (alpha == 0f) {
                view.setAlpha(0f);
                view.setVisibility(View.GONE);
            } else if (alpha == 1f) {
                view.setAlpha(1f);
                view.setVisibility(View.VISIBLE);
            } else {
                view.setVisibility(View.VISIBLE);
                view.setAlpha(alpha);
            }
        }
    }

    public void tap() {
        if (canNot()) {
            return;
        }
        isFullAlpha = !isFullAlpha;

        for (View view : views) {
            view.setVisibility(isFullAlpha ? View.VISIBLE : View.GONE);
        }
    }
}
