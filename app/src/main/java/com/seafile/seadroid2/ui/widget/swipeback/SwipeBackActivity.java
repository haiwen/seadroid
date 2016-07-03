package com.seafile.seadroid2.ui.widget.swipeback;

import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.os.Build;
import android.text.TextUtils;
import android.util.DisplayMetrics;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.activity.BaseActivity;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

/**
 * Base class for swipe back feature
 */
public class SwipeBackActivity extends BaseActivity implements SwipeBackLayout.SwipeBackListener {
    private View mPreview;

    private float mInitOffset;
    private boolean hideTitle = true;
    private int titleResId = -1;

    private String mBitmapId;

    @Override
    public void setContentView(int layoutResID) {
        super.setContentView(R.layout.swipe_back_layout);

        DisplayMetrics metrics = getResources().getDisplayMetrics();
        LayoutInflater inflater = LayoutInflater.from(this);
        mInitOffset = -(1.f / 3) * metrics.widthPixels;

        mPreview = findViewById(R.id.iv_preview);
        FrameLayout contentView = (FrameLayout) findViewById(R.id.content_view);

        if (!hideTitle) {
            int resId = -1 == titleResId ? R.layout.toolbar_actionbar : titleResId;
            inflater.inflate(resId, contentView);
        }

        FrameLayout.LayoutParams lp = new FrameLayout.LayoutParams(MATCH_PARENT,
                MATCH_PARENT, Gravity.BOTTOM);
        final int marginTop = hideTitle ? 0 : (int) (metrics.density * 48.f + .5f);
        lp.setMargins(0, marginTop, 0, 0);
        contentView.addView(inflater.inflate(layoutResID, null), lp);

        final SwipeBackLayout slideLayout = (SwipeBackLayout) findViewById(R.id.swipe_back_layout);
        slideLayout.setShadowResource(R.drawable.sliding_back_shadow);
        slideLayout.setSlidingListener(this);
        slideLayout.setEdgeSize((int) (metrics.density * 20));

        if (getIntent() == null || getIntent().getExtras() == null)
            return;

        mBitmapId = getIntent().getExtras().getString("bitmap_id");

        if (TextUtils.isEmpty(mBitmapId))
            return;

        Bitmap bitmap = IntentUtils.getInstance().getBitmap(mBitmapId);
        if (null != bitmap) {
            if (Build.VERSION.SDK_INT >= 16) {
                mPreview.setBackground(new BitmapDrawable(bitmap));
            } else {
                mPreview.setBackgroundDrawable(new BitmapDrawable(bitmap));
            }

            IntentUtils.getInstance().setIsDisplayed(mBitmapId, true);
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        IntentUtils.getInstance().setIsDisplayed(mBitmapId, false);
    }

    @Override
    public void setTitle(CharSequence title) {
        super.setTitle(title);
        //((TextView) findViewById(R.id.label_title)).setText(title);
    }

    @Override
    public void setTitle(int titleId) {
        super.setTitle(titleId);
        //((TextView) findViewById(R.id.label_title)).setText(titleId);
    }

    @Override
    public void onPanelSlide(View panel, float slideOffset) {
        if (slideOffset <= 0) {
        } else if (slideOffset < 1) {
            mPreview.setTranslationX(mInitOffset * (1 - slideOffset));
        } else {
            mPreview.setTranslationX(0);
            finish();
            overridePendingTransition(0, 0);
        }
    }

    protected void setContentView(int layoutResID, int titleResId) {
        this.titleResId = titleResId;
        setContentView(layoutResID);
    }

    protected void setContentView(int layoutResID, boolean hideTitle) {
        this.hideTitle = hideTitle;
        setContentView(layoutResID);
    }
}
