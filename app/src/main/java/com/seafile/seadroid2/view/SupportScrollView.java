package com.seafile.seadroid2.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.ScrollView;

public class SupportScrollView extends ScrollView {
    public SupportScrollView(Context context) {
        super(context);
    }

    public SupportScrollView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public SupportScrollView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public SupportScrollView(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }

    private void init(){

    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        return super.onTouchEvent(ev);
    }

    public boolean isAtTop() {
        return this.getScrollY() == 0;
    }

    public boolean isAtBottom() {
        View child = this.getChildAt(0);
        if (child == null) return false;

        int diff = (child.getBottom() - (this.getHeight() + this.getScrollY()));
        return diff <= 0;
    }
}
