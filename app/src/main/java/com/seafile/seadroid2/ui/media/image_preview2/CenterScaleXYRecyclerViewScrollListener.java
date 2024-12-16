package com.seafile.seadroid2.ui.media.image_preview2;

import android.content.Context;
import android.os.Handler;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.ScreenUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.SLogs;

public class CenterScaleXYRecyclerViewScrollListener extends RecyclerView.OnScrollListener {

    private float maxScale = 1.2f;
    private float minScale = 1.0f;

    private final int centerX;
    private final float maxDistance;// 12dp is (view width / 2) + margin(2dp)
    private final int itemWidth, itemMargin;

    private LinearLayoutManager layoutManager;

    private final Handler handler = new Handler();
    private boolean isPendingUpdate = false;

    public CenterScaleXYRecyclerViewScrollListener(Context context) {
        itemWidth = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        itemMargin = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_margin);
        maxDistance = (float) itemWidth / 2 + itemMargin;
        centerX = ScreenUtils.getAppScreenWidth() / 2;
    }

    public CenterScaleXYRecyclerViewScrollListener(Context context, float maxScale, float minScale) {
        this.maxScale = maxScale;
        this.minScale = minScale;

        itemWidth = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        itemMargin = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_margin);
        maxDistance = (float) itemWidth / 2 + itemMargin;
        centerX = ScreenUtils.getAppScreenWidth() / 2;

    }

    @Override
    public void onScrollStateChanged(@NonNull RecyclerView recyclerView, int newState) {
        super.onScrollStateChanged(recyclerView, newState);
        if (newState == RecyclerView.SCROLL_STATE_IDLE) {
            adjustChildScale(recyclerView, 0); // 滚动停止时更新缩放状态
        }
    }

    @Override
    public void onScrolled(@NonNull RecyclerView recyclerView, int dx, int dy) {
        super.onScrolled(recyclerView, dx, dy);
//        if (isPendingUpdate) return;
//        isPendingUpdate = true;

        adjustChildScale(recyclerView, dx);

//        handler.postDelayed(() -> {
//            adjustChildScale(recyclerView, dx);
//
//            isPendingUpdate = false;
//        }, 25);
    }

    private void adjustChildScale(RecyclerView recyclerView, int dx) {
        if (layoutManager == null) {
            layoutManager = (LinearLayoutManager) recyclerView.getLayoutManager();
        }

        if (layoutManager == null) {
            throw new IllegalStateException("LayoutManager is null");
        }

        int firstVisiblePosition = layoutManager.findFirstVisibleItemPosition();
        int lastVisiblePosition = layoutManager.findLastVisibleItemPosition();
        if (firstVisiblePosition == RecyclerView.NO_POSITION || lastVisiblePosition == RecyclerView.NO_POSITION) {
            return; // 防止意外状态
        }

        for (int i = firstVisiblePosition; i <= lastVisiblePosition; i++) {
            View view = layoutManager.findViewByPosition(i);
            if (view == null) {
                continue;
            }

            int left = view.getLeft();
            if (left == 0) {
                continue;
            }

            // 计算每个 item 的中心点与屏幕中心的距离
            int viewCenterX = left + itemWidth / 2;

            float distanceFromCenter = Math.abs(centerX - viewCenterX);

            // 1.2f - 4/60 * 0.2f
            float scale = maxScale - (distanceFromCenter / maxDistance) * (maxScale - minScale);
            scale = Math.max(minScale, scale); // 确保不小于最小缩放比例

//            if (i == firstVisiblePosition) {
//                SLogs.d("firstVisiblePosition: " + firstVisiblePosition + ", itemWidth: " + itemWidth + ", left: " + left + ", viewCenterX: " + viewCenterX + ", centerX: " + centerX + ", distanceFromCenter: " + distanceFromCenter + ", scale: " + scale);
//            }

            float alpha = 1.0f - (distanceFromCenter / maxDistance) * 0.8f; // 透明度范围 1.0 到 0.6
            alpha = Math.max(0.8f, alpha);
            view.setAlpha(alpha);

            // 仅当 scale 有变化时才更新
            if (Math.abs(view.getScaleX() - scale) > 0.01f) {
                view.setScaleX(scale);
                view.setScaleY(scale);
            }
        }
    }

}
