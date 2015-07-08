package com.seafile.seadroid2.ui;

import android.app.Activity;
import com.hannesdorfmann.swipeback.SwipeBack;
import com.hannesdorfmann.swipeback.transformer.DefaultSwipeBackTransformer;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.util.Utils;

/**
 * responsible to manipulate the swipe back view according to its current state (opening or closing)
 * with some nice animations.
 */
public class CustomSwipebackTransformer extends DefaultSwipeBackTransformer {
    private BrowserActivity mActivity;

    public CustomSwipebackTransformer(BrowserActivity activity) {
        this.mActivity = activity;
    }

    @Override
    public void onSwipeBackCompleted(SwipeBack swipeBack, Activity activity) {
        if (mActivity == null) return;

        if (mActivity.getCurrentSelectedItem() == mActivity.FILES_VIEW && mActivity.getCurrentPosition() == 0) {
            if (mActivity.getNavContext().inRepo()) {
                if (mActivity.getNavContext().isRepoRoot()) {
                    mActivity.getNavContext().setRepoID(null);
                    mActivity.getSupportActionBar().setTitle(R.string.app_name);
                } else {
                    String parentPath = Utils.getParentPath(mActivity.getNavContext()
                            .getDirPath());
                    mActivity.getNavContext().setDir(parentPath, null);
                    if (parentPath.equals(mActivity.ACTIONBAR_PARENT_PATH)) {
                        mActivity.getSupportActionBar().setTitle(mActivity.getNavContext().getRepoName());
                    }else {
                        mActivity.getSupportActionBar().setTitle(parentPath.substring(parentPath.lastIndexOf(mActivity.ACTIONBAR_PARENT_PATH) + 1));
                    }
                }
                mActivity.getReposFragment().refreshView();

            }
        }
    }
}
