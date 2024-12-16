package com.seafile.seadroid2.ui.media.image_preview2;

import androidx.viewpager2.widget.ViewPager2;

import com.seafile.seadroid2.framework.util.SLogs;

public class PagerSnapBinders {
    private static int whoScroll = -1;

    public static void bind(ViewPager2 pager, GravitySnapHelper snapHelper) {

        pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                if (whoScroll == 1) {
                    whoScroll = -1;
                    return;
                }

                whoScroll = 0;

                SLogs.e("currentPagerPosition: " + position);
                position++;
                snapHelper.smoothScrollToPosition(position);
            }
        });

        snapHelper.setSnapListener(new GravitySnapHelper.SnapListener() {
            @Override
            public void onSnap(int snapPosition) {

                if (whoScroll == 0) {
                    whoScroll = -1;
                    return;
                }

                whoScroll = 1;

                SLogs.e("currentSnapPosition: " + snapPosition);

                snapPosition--;
                pager.setCurrentItem(snapPosition, true);
            }
        });
    }

}
