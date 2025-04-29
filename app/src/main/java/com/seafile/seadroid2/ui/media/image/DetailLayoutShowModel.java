package com.seafile.seadroid2.ui.media.image;

import com.seafile.seadroid2.view.photoview.ScrollDirection;
import com.seafile.seadroid2.view.photoview.ScrollStatus;

public class DetailLayoutShowModel {
    public int distance;
    public ScrollDirection direction;
    public ScrollStatus status;
    public boolean isShowing;

    public DetailLayoutShowModel(int distance, ScrollDirection direction, ScrollStatus status, boolean isShowing) {
        this.distance = distance;
        this.direction = direction;
        this.status = status;
        this.isShowing = isShowing;
    }
}
