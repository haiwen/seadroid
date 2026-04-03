package com.seafile.seadroid2.view;

import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Rect;
import android.view.View;

import androidx.annotation.ColorInt;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.SizeUtils;

public class LeftMarginDividerItemDecoration extends RecyclerView.ItemDecoration {

    private Paint mPaint;
    private int lingHeight = 0;
    private int lineMarginLeft = 0;

    public LeftMarginDividerItemDecoration(int leftMargin, @ColorInt int color) {
        mPaint = new Paint();
        mPaint.setAntiAlias(true);
        mPaint.setColor(color);
        lingHeight = SizeUtils.dp2px(1);
        lineMarginLeft = leftMargin;
    }

    @Override
    public void getItemOffsets(Rect outRect, View view, RecyclerView parent, RecyclerView.State state) {
        super.getItemOffsets(outRect, view, parent, state);

        outRect.bottom = lingHeight;
    }

    @Override
    public void onDrawOver(@NonNull Canvas c, @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
        super.onDrawOver(c, parent, state);

        final int childCount = parent.getChildCount();
        for (int i = 0; i < childCount; i++) {
            final View child = parent.getChildAt(i);
            final RecyclerView.LayoutParams params = (RecyclerView.LayoutParams) child
                    .getLayoutParams();
            final int top = child.getBottom() + params.bottomMargin;
            final int bottom = top + lingHeight;
            final int left = child.getLeft() + lineMarginLeft;
            final int right = child.getWidth() + left - lineMarginLeft;
            c.drawRect(left, top, right, bottom, mPaint);
        }
    }
}
