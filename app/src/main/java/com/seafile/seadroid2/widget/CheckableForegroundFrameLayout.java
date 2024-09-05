package com.seafile.seadroid2.widget;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.Checkable;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.stream.IntStream;


public class CheckableForegroundFrameLayout extends FrameLayout implements Checkable {
    public CheckableForegroundFrameLayout(@NonNull Context context) {
        super(context);
    }

    public CheckableForegroundFrameLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public CheckableForegroundFrameLayout(@NonNull Context context, @Nullable AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    private boolean _isChecked = false;

    @Override
    public void setChecked(boolean checked) {
        if (_isChecked == checked) {
            return;
        }
        _isChecked = checked;

        refreshDrawableState();
    }

    @Override
    public boolean isChecked() {
        return _isChecked;
    }

    @Override
    public void toggle() {
        _isChecked = !_isChecked;
        refreshDrawableState();
    }

    private final int[] CHECKED_STATE_SET = IntStream.of(android.R.attr.state_checked).toArray();

    @Override
    protected int[] onCreateDrawableState(int extraSpace) {
        int[] retArray = super.onCreateDrawableState(extraSpace + 1);
        if (_isChecked) {
            mergeDrawableStates(retArray, CHECKED_STATE_SET);
        }
        return retArray;
    }
}
