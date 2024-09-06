package com.seafile.seadroid2.widget;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.graphics.Canvas;
import android.graphics.PorterDuff;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.widget.FrameLayout;

import androidx.annotation.AttrRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.annotation.StyleRes;
import androidx.appcompat.widget.TintTypedArray;
import androidx.core.graphics.drawable.DrawableCompat;

public class ForegroundHelper {
    private static final int[] STYLEABLE = new int[]{16843017, 16843264, 16843886, 16843885};
    private static final int STYLEABLE_ANDROID_FOREGROUND = 0;
    private static final int STYLEABLE_ANDROID_FOREGROUND_GRAVITY = 1;
    private static final int STYLEABLE_ANDROID_FOREGROUND_TINT_MODE = 2;
    private static final int STYLEABLE_ANDROID_FOREGROUND_TINT = 3;
    @NonNull
    private final View mView;
    private boolean mHasFrameworkImplementation;
    @Nullable
    private ForegroundInfo mForegroundInfo;

    public ForegroundHelper(@NonNull View view) {
        this.mView = view;
    }

    @SuppressLint({"RestrictedApi"})
    public void init(@NonNull Context context, @Nullable AttributeSet attrs, @AttrRes int defStyleAttr, @StyleRes int defStyleRes) {
        this.mHasFrameworkImplementation = this.mView instanceof FrameLayout || Build.VERSION.SDK_INT >= 23 && context.getApplicationInfo().targetSdkVersion >= 23;
        if (!this.mHasFrameworkImplementation) {
            TintTypedArray a = TintTypedArray.obtainStyledAttributes(context, attrs, STYLEABLE, defStyleAttr, defStyleRes);
            if (a.hasValue(0)) {
                this.setSupportForeground(a.getDrawable(0));
            }

            if (a.hasValue(1)) {
                this.setSupportForegroundGravity(a.getInt(1, 0));
            }

            if (a.hasValue(2)) {
                this.setSupportForegroundTintMode(MoreDrawableCompat.parseTintMode(a.getInt(2, -1), (PorterDuff.Mode)null));
            }

            if (a.hasValue(3)) {
                this.setSupportForegroundTintList(a.getColorStateList(3));
            }

            a.recycle();
        }
    }

    @RequiresApi(24)
    public void onVisibilityAggregated(boolean isVisible) {
        if (!this.mHasFrameworkImplementation) {
            Drawable fg = this.mForegroundInfo != null ? this.mForegroundInfo.mDrawable : null;
            if (fg != null && isVisible != fg.isVisible()) {
                fg.setVisible(isVisible, false);
            }

        }
    }

    public void draw(@NonNull Canvas canvas) {
        if (!this.mHasFrameworkImplementation) {
            this.onDrawForeground(canvas);
        }
    }

    public void onRtlPropertiesChanged(int layoutDirection) {
        if (!this.mHasFrameworkImplementation) {
            this.resolveForegroundDrawable(layoutDirection);
        }
    }

    private void resolveForegroundDrawable(int layoutDirection) {
        if (this.mForegroundInfo != null && this.mForegroundInfo.mDrawable != null) {
            DrawableCompat.setLayoutDirection(this.mForegroundInfo.mDrawable, layoutDirection);
        }

    }

    protected boolean verifyDrawable(@NonNull Drawable who) {
        if (this.mHasFrameworkImplementation) {
            return false;
        } else {
            return this.mForegroundInfo != null && this.mForegroundInfo.mDrawable == who;
        }
    }

    public void drawableStateChanged() {
        if (!this.mHasFrameworkImplementation) {
            int[] state = this.mView.getDrawableState();
            boolean changed = false;
            Drawable fg = this.mForegroundInfo != null ? this.mForegroundInfo.mDrawable : null;
            if (fg != null && fg.isStateful()) {
                changed |= fg.setState(state);
            }

            if (changed) {
                this.mView.invalidate();
            }

        }
    }

    public void drawableHotspotChanged(float x, float y) {
        if (!this.mHasFrameworkImplementation) {
            if (this.mForegroundInfo != null && this.mForegroundInfo.mDrawable != null) {
                this.mForegroundInfo.mDrawable.setHotspot(x, y);
            }

        }
    }

    public void jumpDrawablesToCurrentState() {
        if (!this.mHasFrameworkImplementation) {
            if (this.mForegroundInfo != null && this.mForegroundInfo.mDrawable != null) {
                this.mForegroundInfo.mDrawable.jumpToCurrentState();
            }

        }
    }

    @Nullable
    public Drawable getSupportForeground() {
        return this.mForegroundInfo != null ? this.mForegroundInfo.mDrawable : null;
    }

    public void setSupportForeground(@Nullable Drawable foreground) {
        if (this.mForegroundInfo == null) {
            if (foreground == null) {
                return;
            }

            this.mForegroundInfo = new ForegroundInfo();
        }

        if (foreground != this.mForegroundInfo.mDrawable) {
            if (this.mForegroundInfo.mDrawable != null) {
                if (this.mView.isAttachedToWindow()) {
                    this.mForegroundInfo.mDrawable.setVisible(false, false);
                }

                this.mForegroundInfo.mDrawable.setCallback((Drawable.Callback)null);
                this.mView.unscheduleDrawable(this.mForegroundInfo.mDrawable);
            }

            this.mForegroundInfo.mDrawable = foreground;
            if (foreground != null) {
                this.mView.setWillNotDraw(false);
                DrawableCompat.setLayoutDirection(foreground, this.mView.getLayoutDirection());
                if (foreground.isStateful()) {
                    foreground.setState(this.mView.getDrawableState());
                }

                this.applyForegroundTint();
                if (this.mView.isAttachedToWindow()) {
                    foreground.setVisible(this.mView.getWindowVisibility() == View.VISIBLE && this.mView.isShown(), false);
                }

                foreground.setCallback(this.mView);
            }

            this.mView.requestLayout();
            this.mView.invalidate();
        }
    }

    public int getSupportForegroundGravity() {
        return this.mForegroundInfo != null ? this.mForegroundInfo.mGravity : 8388659;
    }

    public void setSupportForegroundGravity(int gravity) {
        if (this.mForegroundInfo == null) {
            this.mForegroundInfo = new ForegroundInfo();
        }

        if (this.mForegroundInfo.mGravity != gravity) {
            if ((gravity & 8388615) == 0) {
                gravity |= 8388611;
            }

            if ((gravity & 112) == 0) {
                gravity |= 48;
            }

            this.mForegroundInfo.mGravity = gravity;
            this.mView.requestLayout();
        }

    }

    public void setSupportForegroundTintList(@Nullable ColorStateList tint) {
        if (this.mForegroundInfo == null) {
            this.mForegroundInfo = new ForegroundInfo();
        }

        if (this.mForegroundInfo.mTintInfo == null) {
            this.mForegroundInfo.mTintInfo = new TintInfo();
        }

        this.mForegroundInfo.mTintInfo.mTintList = tint;
        this.mForegroundInfo.mTintInfo.mHasTintList = true;
        this.applyForegroundTint();
    }

    @Nullable
    public ColorStateList getSupportForegroundTintList() {
        return this.mForegroundInfo != null && this.mForegroundInfo.mTintInfo != null ? this.mForegroundInfo.mTintInfo.mTintList : null;
    }

    public void setSupportForegroundTintMode(@Nullable PorterDuff.Mode tintMode) {
        if (this.mForegroundInfo == null) {
            this.mForegroundInfo = new ForegroundInfo();
        }

        if (this.mForegroundInfo.mTintInfo == null) {
            this.mForegroundInfo.mTintInfo = new TintInfo();
        }

        this.mForegroundInfo.mTintInfo.mTintMode = tintMode;
        this.mForegroundInfo.mTintInfo.mHasTintMode = true;
        this.applyForegroundTint();
    }

    @Nullable
    public PorterDuff.Mode getSupportForegroundTintMode() {
        return this.mForegroundInfo != null && this.mForegroundInfo.mTintInfo != null ? this.mForegroundInfo.mTintInfo.mTintMode : null;
    }

    private void applyForegroundTint() {
        if (this.mForegroundInfo != null && this.mForegroundInfo.mDrawable != null && this.mForegroundInfo.mTintInfo != null) {
            TintInfo tintInfo = this.mForegroundInfo.mTintInfo;
            if (tintInfo.mHasTintList || tintInfo.mHasTintMode) {
                this.mForegroundInfo.mDrawable = this.mForegroundInfo.mDrawable.mutate();
                if (tintInfo.mHasTintList) {
                    this.mForegroundInfo.mDrawable.setTintList(tintInfo.mTintList);
                }

                if (tintInfo.mHasTintMode) {
                    this.mForegroundInfo.mDrawable.setTintMode(tintInfo.mTintMode);
                }

                if (this.mForegroundInfo.mDrawable.isStateful()) {
                    this.mForegroundInfo.mDrawable.setState(this.mView.getDrawableState());
                }
            }
        }

    }

    private void onDrawForeground(@NonNull Canvas canvas) {
        Drawable foreground = this.mForegroundInfo != null ? this.mForegroundInfo.mDrawable : null;
        if (foreground != null) {
            Rect selfBounds = this.mForegroundInfo.mSelfBounds;
            Rect overlayBounds = this.mForegroundInfo.mOverlayBounds;
            selfBounds.set(0, 0, this.mView.getWidth(), this.mView.getHeight());
            int ld = this.mView.getLayoutDirection();
            Gravity.apply(this.mForegroundInfo.mGravity, foreground.getIntrinsicWidth(), foreground.getIntrinsicHeight(), selfBounds, overlayBounds, ld);
            foreground.setBounds(overlayBounds);
            foreground.draw(canvas);
        }

    }

    private static class ForegroundInfo {
        public Drawable mDrawable;
        public TintInfo mTintInfo;
        public int mGravity;
        public final Rect mSelfBounds;
        public final Rect mOverlayBounds;

        private ForegroundInfo() {
            this.mGravity = 119;
            this.mSelfBounds = new Rect();
            this.mOverlayBounds = new Rect();
        }
    }

    private static class TintInfo {
        public ColorStateList mTintList;
        public PorterDuff.Mode mTintMode;
        public boolean mHasTintMode;
        public boolean mHasTintList;

        private TintInfo() {
        }
    }
}