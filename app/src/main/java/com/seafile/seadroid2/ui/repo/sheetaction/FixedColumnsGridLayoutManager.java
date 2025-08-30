package com.seafile.seadroid2.ui.repo.sheetaction;

import android.content.Context;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

public class FixedColumnsGridLayoutManager extends GridLayoutManager {
    private int itemWidth = 0;
    private int visibleColumnCount = 6;

    public FixedColumnsGridLayoutManager(Context context, int spanCount, int orientation, boolean reverseLayout) {
        super(context, spanCount, orientation, reverseLayout);
    }

    public void setVisibleColumnCount(int count) {
        this.visibleColumnCount = count;
    }

    @Override
    public void onLayoutChildren(RecyclerView.Recycler recycler, RecyclerView.State state) {
        if (itemWidth == 0 && getChildCount() > 0) {
            View firstChild = getChildAt(0);
            if (firstChild != null) {
                itemWidth = firstChild.getMeasuredWidth();
            }
        }
        super.onLayoutChildren(recycler, state);
    }

    @Override
    public void onMeasure(RecyclerView.Recycler recycler, RecyclerView.State state, int widthSpec, int heightSpec) {
        if (itemWidth == 0 && state.getItemCount() > 0) {
            View view = recycler.getViewForPosition(0);
            addView(view);
            measureChildWithMargins(view, 0, 0);
            itemWidth = getDecoratedMeasuredWidth(view);
            removeAndRecycleView(view, recycler);
        }

        if (itemWidth > 0) {
            int width = itemWidth * visibleColumnCount;
            widthSpec = View.MeasureSpec.makeMeasureSpec(width, View.MeasureSpec.EXACTLY);
        }

        super.onMeasure(recycler, state, widthSpec, heightSpec);
    }
}
