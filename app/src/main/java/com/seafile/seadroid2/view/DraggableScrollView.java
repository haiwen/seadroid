package com.seafile.seadroid2.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.widget.ScrollView;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.view.photoview.CustomSingleGestureDetector;
import com.seafile.seadroid2.view.photoview.OnGestureListener;
import com.seafile.seadroid2.view.photoview.OnViewActionEndListener;
import com.seafile.seadroid2.view.photoview.OnViewDragListener;
import com.seafile.seadroid2.view.photoview.ScrollDirection;

public class DraggableScrollView extends ScrollView {
    public DraggableScrollView(Context context) {
        super(context);
        init();
    }

    public DraggableScrollView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public DraggableScrollView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    public DraggableScrollView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
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

    private float mLastY;
    private boolean mIsTopReached = false;

    @Override
    public boolean onTouchEvent(@NonNull MotionEvent ev) {
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mLastY = ev.getY();
                mIsTopReached = (getScrollY() == 0);

                if (mDragDetector != null) {
                    mDragDetector.onTouchEvent(ev);
                }
                break;
            case MotionEvent.ACTION_MOVE:
                if (mDragDetector != null) {
                    if (mDragDetector.isDragging()) {
                        mDragDetector.onTouchEvent(ev);
                        return true;
                    }
                }
                float deltaY = mLastY - ev.getY();
                mLastY = ev.getY();
                if (deltaY < 0) { // 下滑
                    if (getScrollY() <= 0) {
                        setEnabled(false);
                        setVerticalScrollBarEnabled(false);

                        if (mDragDetector != null) {
                            mDragDetector.onTouchEvent(ev);
                        }
                        return true;

                    }
                }
                break;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_CANCEL:
                setEnabled(true);
                setVerticalScrollBarEnabled(true);
                mIsTopReached = false;
                reset();
                if (mDragDetector != null) {
                    mDragDetector.onTouchEvent(ev);
                }
                if (mOnViewActionEndListener != null) {
                    mOnViewActionEndListener.onEnd();
                }
                break;
        }

        return super.onTouchEvent(ev);
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

            // Determine the direction first: when the accumulated slippage exceeds the threshold
            if (mScrollDirection == ScrollDirection.NONE) {
                float distance = (float) Math.sqrt(mTotalDragX * mTotalDragX + mTotalDragY * mTotalDragY);
                if (distance >= mDragDetector.getTouchSlop()) {
                    mScrollDirection = (mTotalDragX > mTotalDragY) ? ScrollDirection.HORIZONTAL : ScrollDirection.VERTICAL;
                }
            }

            // Determine the drag direction
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
