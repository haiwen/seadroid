package com.seafile.seadroid2.ui.media.image_preview2;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.view.photoview.CustomSingleGestureDetector;
import com.seafile.seadroid2.view.photoview.OnGestureListener;
import com.seafile.seadroid2.view.photoview.OnViewActionEndListener;
import com.seafile.seadroid2.view.photoview.OnViewDragListener;
import com.seafile.seadroid2.view.photoview.ScrollDirection;

public class CustomFrameLayout extends FrameLayout {
    public CustomFrameLayout(@NonNull Context context) {
        super(context);
        init();
    }

    public CustomFrameLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public CustomFrameLayout(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    private void init() {
        mDragDetector = new CustomSingleGestureDetector(getContext(), onGestureListener);
    }

    private CustomSingleGestureDetector mDragDetector;
    private OnViewDragListener mOnViewDragListener;

    private OnViewActionEndListener mOnViewActionEndListener;

    public void setOnViewActionEndListener(OnViewActionEndListener listener) {
        mOnViewActionEndListener = listener;
    }

    public void setOnViewDragListener(OnViewDragListener listener) {
        mOnViewDragListener = listener;
    }

    @Override
    public boolean onTouchEvent(@NonNull MotionEvent ev) {

        if (ev.getAction() == MotionEvent.ACTION_UP || ev.getAction() == MotionEvent.ACTION_CANCEL) {
            if (mOnViewActionEndListener != null) {
                mOnViewActionEndListener.onEnd();
            }
            reset();
        }

        boolean handled = false;

        // Try the Scale/Drag detector
        if (mDragDetector != null) {
            handled = mDragDetector.onTouchEvent(ev);
        }

        return handled;
    }

    private float mTotalDragX;
    private float mTotalDragY;
    private ScrollDirection mScrollDirection = ScrollDirection.NONE;

    private void reset() {
        mTotalDragX = mTotalDragY = 0;
        mScrollDirection = ScrollDirection.NONE;
    }

    private final OnGestureListener onGestureListener = new OnGestureListener() {
        @Override
        public void onDrag(float dx, float dy) {
            mTotalDragX += Math.abs(dx);
            mTotalDragY += Math.abs(dy);
            // 当累计滑动超过阈值时确定方向
            if (mScrollDirection == ScrollDirection.NONE) {
                float distance = (float) Math.sqrt(mTotalDragX * mTotalDragX + mTotalDragY * mTotalDragY);
                if (distance >= mDragDetector.getTouchSlop()) {
                    mScrollDirection = (mTotalDragX > mTotalDragY) ? ScrollDirection.HORIZONTAL : ScrollDirection.VERTICAL;
                }
            }

            if (mOnViewDragListener != null) {
                if (mScrollDirection == ScrollDirection.VERTICAL) {
                    if (dy < -1f) {
                        mOnViewDragListener.onDrag(ScrollDirection.UP, dx, dy);
                    } else if (dy > 1f) {
                        mOnViewDragListener.onDrag(ScrollDirection.DOWN, dx, dy);
                    }
                } else if (mScrollDirection == ScrollDirection.HORIZONTAL) {
                    if (dx < -1f) {
                        mOnViewDragListener.onDrag(ScrollDirection.LEFT, dx, dy);
                    } else if (dx > 1f) {
                        mOnViewDragListener.onDrag(ScrollDirection.RIGHT, dx, dy);
                    }
                }
            }
        }

        @Override
        public void onFling(float startX, float startY, float velocityX, float velocityY) {

        }

        @Override
        public void onScale(float scaleFactor, float focusX, float focusY) {

        }

        @Override
        public void onScale(float scaleFactor, float focusX, float focusY, float dx, float dy) {

        }
    };

}
