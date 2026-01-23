package com.seafile.seadroid2.ui.media.image;

import com.seafile.seadroid2.view.photoview.ScrollDirection;

public interface OnDetailLayoutScrollChangeListener {
    void onCancel(ScrollDirection direction, int distance, boolean isShowing);

    void onFinish(ScrollDirection direction, int distance, boolean isShowing);

    void onScroll(ScrollDirection direction, int distance, boolean isShowing, float alpha);

}
