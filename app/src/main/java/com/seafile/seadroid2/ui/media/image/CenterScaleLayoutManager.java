package com.seafile.seadroid2.ui.media.image;

import android.content.Context;
import android.graphics.PointF;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.LinearSmoothScroller;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.ScreenUtils;
import com.blankj.utilcode.util.SizeUtils;

public class CenterScaleLayoutManager extends LinearLayoutManager {

    private static final float DEFAULT_MAX_SCALE = 1.2f;
    private float maxScale = DEFAULT_MAX_SCALE;

    private OnCenterItemChangedListener centerItemChangedListener;
    private int lastCenterPosition = RecyclerView.NO_POSITION;
    private Context context;

    public CenterScaleLayoutManager(Context context) {
        super(context, HORIZONTAL, false);
        this.context = context;
    }

    private RecyclerView recyclerView;

    @Override
    public void onAttachedToWindow(RecyclerView view) {
        super.onAttachedToWindow(view);
        this.recyclerView = view;
    }

    @Override
    public void onDetachedFromWindow(RecyclerView view, RecyclerView.Recycler recycler) {
        super.onDetachedFromWindow(view, recycler);
        recyclerView = null;
    }

    public Context getContext() {
        return context;
    }

    public void setOnCenterItemChangedListener(OnCenterItemChangedListener listener) {
        this.centerItemChangedListener = listener;
    }

    @Override
    public void onLayoutChildren(@NonNull RecyclerView.Recycler recycler, @NonNull RecyclerView.State state) {
        super.onLayoutChildren(recycler, state);
        scaleChildren();
    }

    @Override
    public void onScrollStateChanged(int state) {
        super.onScrollStateChanged(state);
        if (state == RecyclerView.SCROLL_STATE_IDLE) {
            // 滑动停止时，如果中心位置有变化，触发回调
            if (centerItemChangedListener != null && currentCenterPosition != RecyclerView.NO_POSITION
                    && currentCenterPosition != lastCenterPosition) {
                centerItemChangedListener.onCenterItemChanged(currentCenterPosition);
                lastCenterPosition = currentCenterPosition;
            }
        }
    }

    @Override
    public int scrollHorizontallyBy(int dx, RecyclerView.Recycler recycler, RecyclerView.State state) {
        int scrolled = super.scrollHorizontallyBy(dx, recycler, state);
        scaleChildren();
        return scrolled;
    }

    private final float MIN_SCALE = 1.0f;
    private final float MIN_ALPHA = 0.8f;
    private final float maxDistance = SizeUtils.dp2px(12);//half of the item width
    private int currentCenterPosition = RecyclerView.NO_POSITION;

    private void scaleChildren() {
        // gets the range of visible items
        int firstVisible = findFirstVisibleItemPosition();
        int lastVisible = findLastVisibleItemPosition();

        if (firstVisible == RecyclerView.NO_POSITION || lastVisible == RecyclerView.NO_POSITION)
            return;

        // calculate the center point of the recyclerview
        int recyclerCenterX = (getWidth() - getPaddingStart() - getPaddingEnd()) / 2 + getPaddingStart();


        // Record the location of the most recent item (for callbacks)
        View closestChild = null;
        int minDistance = Integer.MAX_VALUE;
        int closestPosition = RecyclerView.NO_POSITION;

        // iterate through all visible items and scale them based on their distance from the center
        for (int i = firstVisible; i <= lastVisible; i++) {
            View child = findViewByPosition(i);
            if (child == null) continue;

            // calculate the center point of the item
            int childCenterX = (child.getLeft() + child.getRight()) / 2;

            // Calculate the distance from the center of the item to the center of the RecyclerView
            float distanceFromCenter = Math.abs(childCenterX - recyclerCenterX);

            // The scale value is calculated based on the distance
            float scale = maxScale - (distanceFromCenter / maxDistance) * (maxScale - MIN_SCALE);
            scale = Math.max(MIN_SCALE, scale); // Make sure it's not less than the minimum zoom ratio

//            // calculate transparency
//            float alpha = 1.0f - (distanceFromCenter / maxDistance) * (1.0f - MIN_ALPHA);
//            alpha = Math.max(MIN_ALPHA, alpha);
//            child.setAlpha(alpha);

            if (Math.abs(child.getScaleX() - scale) > 0.01f) {
                child.setScaleX(scale);
                child.setScaleY(scale);
            }

            // 记录最近的item（用于回调）
            if (distanceFromCenter < minDistance) {
                minDistance = (int) distanceFromCenter;
                closestChild = child;
                closestPosition = i;
            }
        }

        // 更新当前中心位置，但不触发回调
        if (closestChild != null) {
            currentCenterPosition = closestPosition;
        }
//        // 更新中心item的位置（用于回调）
//        if (closestChild != null && centerItemChangedListener != null && closestPosition != lastCenterPosition) {
//            centerItemChangedListener.onCenterItemChanged(closestPosition);
//            lastCenterPosition = closestPosition;
//        }
    }

    /**
     * see CenterSnapHelper
     */
    @Deprecated
    private void smoothCenterChild() {
        if (getChildCount() == 0) return;

        int recyclerCenterX = (getWidth() - getPaddingStart() - getPaddingEnd()) / 2 + getPaddingStart();
        View closestChild = null;
        int minDistance = Integer.MAX_VALUE;

        for (int i = 0; i < getChildCount(); i++) {
            View child = getChildAt(i);
            int childCenterX = (child.getLeft() + child.getRight()) / 2;
            int distance = Math.abs(childCenterX - recyclerCenterX);

            if (distance < minDistance) {
                minDistance = distance;
                closestChild = child;
            }
        }

        if (closestChild != null) {
            int position = getPosition(closestChild);
            smoothScrollToPositionInternal(position);

            if (centerItemChangedListener != null && position != lastCenterPosition) {
                centerItemChangedListener.onCenterItemChanged(position);
                lastCenterPosition = position;
            }
        }
    }

    private void smoothScrollToPositionInternal(int position) {
        if (position == RecyclerView.NO_POSITION) return;

        CenterSmoothScroller smoothScroller = new CenterSmoothScroller(this);
        smoothScroller.setTargetPosition(position);
        startSmoothScroll(smoothScroller);
    }

    public void scrollToPositionWithCenter(int position) {
        if (position == RecyclerView.NO_POSITION) return;

        if (recyclerView != null) {
            recyclerView.post(() -> smoothScrollToPositionInternal(position));
        }
    }

    private static class CenterSmoothScroller extends LinearSmoothScroller {

        private final CenterScaleLayoutManager layoutManager;

        CenterSmoothScroller(CenterScaleLayoutManager layoutManager) {
            super(layoutManager.getContext());
            this.layoutManager = layoutManager;
        }

        @Override
        public PointF computeScrollVectorForPosition(int targetPosition) {
            return layoutManager.computeScrollVectorForPosition(targetPosition);
        }

        @Override
        public int calculateDtToFit(int viewStart, int viewEnd, int boxStart, int boxEnd, int snapPreference) {
            int viewCenter = (viewStart + viewEnd) / 2;
            int boxCenter = (boxStart + boxEnd) / 2;
            return boxCenter - viewCenter;
        }

        @Override
        protected int calculateTimeForScrolling(int dx) {
            return Math.min(200, super.calculateTimeForScrolling(dx));
        }
    }

    public interface OnCenterItemChangedListener {
        void onCenterItemChanged(int position);
    }
}



