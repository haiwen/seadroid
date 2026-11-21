package com.seafile.seadroid2.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.widget.ScrollView;

public class CustomNestedScrollView extends ScrollView {
    private OnEdgeReachedListener mEdgeListener;

    public interface OnEdgeReachedListener {
        void onTopEdgeReached();

        void onBottomEdgeReached();
    }

    public CustomNestedScrollView(Context context) {
        super(context);
    }

    public CustomNestedScrollView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public CustomNestedScrollView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void setOnEdgeReachedListener(OnEdgeReachedListener listener) {
        mEdgeListener = listener;
    }

    @Override
    protected void onScrollChanged(int l, int t, int oldl, int oldt) {
        super.onScrollChanged(l, t, oldl, oldt);
        if (t == 0 && mEdgeListener != null) {
            mEdgeListener.onTopEdgeReached();
        }else if (t == (getChildAt(0).getHeight() - getHeight()) && mEdgeListener!= null) {
            mEdgeListener.onBottomEdgeReached();
        }
    }

    private float mLastY;
    private boolean mIsTopReached = false;

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        switch (ev.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mLastY = ev.getY();
                mIsTopReached = (getScrollY() == 0);
                break;

            case MotionEvent.ACTION_MOVE:
                float deltaY = mLastY - ev.getY();
                mLastY = ev.getY();

                if (deltaY < 0) { // Swipe down
                    if (getScrollY() <= 0) {
                        // Already at the top; let the parent view handle it
                        getParent().requestDisallowInterceptTouchEvent(false);
                        return false;
                    }
                } else { // Swipe up
                    // Let the parent view handle it if there is no more content to scroll
                    if (getScrollY() + getHeight() >= getChildAt(0).getHeight()) {
                        getParent().requestDisallowInterceptTouchEvent(false);
                        return false;
                    }
                }
                // Handle scrolling here in all other cases
                getParent().requestDisallowInterceptTouchEvent(true);
                break;

            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_CANCEL:
                mIsTopReached = false;
                getParent().requestDisallowInterceptTouchEvent(false);
                break;
        }
        return super.onTouchEvent(ev);
    }
}
