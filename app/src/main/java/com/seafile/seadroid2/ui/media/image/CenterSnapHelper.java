package com.seafile.seadroid2.ui.media.image;

import android.view.View;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.LinearSnapHelper;
import androidx.recyclerview.widget.RecyclerView;

public class CenterSnapHelper extends LinearSnapHelper {

    @Override
    public View findSnapView(RecyclerView.LayoutManager layoutManager) {
        if (!(layoutManager instanceof LinearLayoutManager)) return null;

        int centerX = (layoutManager.getWidth() - layoutManager.getPaddingStart() - layoutManager.getPaddingEnd()) / 2 + layoutManager.getPaddingStart();
        View closestChild = null;
        int minDistance = Integer.MAX_VALUE;

        for (int i = 0; i < layoutManager.getChildCount(); i++) {
            View child = layoutManager.getChildAt(i);
            int childCenterX = (child.getLeft() + child.getRight()) / 2;
            int distance = Math.abs(childCenterX - centerX);

            if (distance < minDistance) {
                minDistance = distance;
                closestChild = child;
            }
        }
        return closestChild;
    }

    @Override
    public int[] calculateDistanceToFinalSnap(@NonNull RecyclerView.LayoutManager layoutManager, @NonNull View targetView) {
        if (!(layoutManager instanceof LinearLayoutManager)) return new int[]{0, 0};

        int centerX = (layoutManager.getWidth() - layoutManager.getPaddingStart() - layoutManager.getPaddingEnd()) / 2 + layoutManager.getPaddingStart();
        int childCenterX = (targetView.getLeft() + targetView.getRight()) / 2;
        int dx = childCenterX - centerX;

        return new int[]{dx, 0};
    }
}
