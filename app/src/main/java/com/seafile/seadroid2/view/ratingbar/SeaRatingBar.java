package com.seafile.seadroid2.view.ratingbar;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.widget.ImageView;
import android.widget.LinearLayout;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class SeaRatingBar extends LinearLayout {
    private RatingStatus mRatingStatus;
    private Drawable mStarFullDrawable;
    private Drawable mStarEmptyDrawable;
    private Drawable mStarHalfDrawable;
    private int mStarWidth;
    private int mStarHeight;
    private int mStarPadding;
    private int mStarTotal;
    private float mRating;
    private OnRatingChangedListener mOnRatingChangedListener;
    private final List<Integer> mBoundaryList = new ArrayList<>(5);

    public SeaRatingBar(Context context) {
        this(context, null);
    }

    public SeaRatingBar(Context context, AttributeSet attrs) {
        this(context, attrs, -1);
    }

    public SeaRatingBar(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(attrs);
    }

    private void init(AttributeSet attrs) {
        setOrientation(HORIZONTAL);

//        TypedArray mTypedArray = getContext().obtainStyledAttributes(attrs, R.styleable.SeaRatingBar);
//        mRatingStatus = RatingStatus.getStatus(mTypedArray.getInt(R.styleable.SeaRatingBar_srb_ratingStatus, RatingStatus.Disable.mStatus));
//        mStarFullDrawable = mTypedArray.getDrawable(R.styleable.SeaRatingBar_srb_starFullResource);
//        mStarEmptyDrawable = mTypedArray.getDrawable(R.styleable.SeaRatingBar_srb_starEmptyResource);
//        mStarHalfDrawable = mTypedArray.getDrawable(R.styleable.SeaRatingBar_srb_starHalfResource);
//
//        mStarWidth = (int) mTypedArray.getDimension(R.styleable.SeaRatingBar_srb_starImageWidth, 24);
//        mStarHeight = (int) mTypedArray.getDimension(R.styleable.SeaRatingBar_srb_starImageHeight, 24);
//        mStarPadding = (int) mTypedArray.getDimension(R.styleable.SeaRatingBar_srb_starImagePadding, 4);
//        mStarTotal = mTypedArray.getInt(R.styleable.SeaRatingBar_srb_starTotal, 5);
//        if (mStarTotal <= 0)
//            throw new IllegalArgumentException("SeaRatingBar Error: starTotal must be positive!");
//
//        mRating = mTypedArray.getFloat(R.styleable.SeaRatingBar_srb_rating, 5f);
//        mTypedArray.recycle();
    }

    public void build() {
        for (int i = 0; i < mStarTotal; i++) {
            addView(createStarImageView(i == mStarTotal - 1));
        }

        setRating(mRating);
    }

    public void setStarFullDrawable(Drawable mStarFullDrawable) {
        this.mStarFullDrawable = mStarFullDrawable;
    }

    public void setStarEmptyDrawable(Drawable mStarEmptyDrawable) {
        this.mStarEmptyDrawable = mStarEmptyDrawable;
    }

    public void setStarHalfDrawable(Drawable mStarHalfDrawable) {
        this.mStarHalfDrawable = mStarHalfDrawable;
    }

    public void setStarWidth(int mStarWidth) {
        this.mStarWidth = mStarWidth;
    }

    public void setStarHeight(int mStarHeight) {
        this.mStarHeight = mStarHeight;
    }

    public void setStarPadding(int mStarPadding) {
        this.mStarPadding = mStarPadding;
    }

    public void setStarTotal(int mStarTotal) {
        this.mStarTotal = mStarTotal;
    }

    private ImageView createStarImageView(boolean isLast) {
        ImageView imageView = new ImageView(getContext());
        LayoutParams layoutParams = new LayoutParams(mStarWidth, mStarHeight);
        layoutParams.setMargins(0, 0, isLast ? 0 : mStarPadding, 0);
        imageView.setLayoutParams(layoutParams);
        return imageView;
    }

    public void setRating(float rating) {
        if (rating > mStarTotal) {
            rating = mStarTotal;
        }
        this.mRating = rating;
        if (mOnRatingChangedListener != null) {
            mOnRatingChangedListener.onRatingChanged(rating);
        }

        int partInteger = (int) Math.floor(rating);
        float partDecimal = new BigDecimal(String.valueOf(rating))
                .subtract(new BigDecimal(String.valueOf(partInteger)))
                .floatValue();

        for (int i = 0; i < partInteger; i++) {
            ((ImageView) getChildAt(i)).setImageDrawable(mStarFullDrawable);
        }

        for (int i = partInteger; i < mStarTotal; i++) {
            ((ImageView) getChildAt(i)).setImageDrawable(mStarEmptyDrawable);
        }

        if (partDecimal >= 0.25) {
            if (partDecimal < 0.75 && mStarHalfDrawable != null) {
                ((ImageView) getChildAt(partInteger)).setImageDrawable(mStarHalfDrawable);
            } else if (partDecimal >= 0.75) {
                ((ImageView) getChildAt(partInteger)).setImageDrawable(mStarFullDrawable);
            }
        }
    }

    private float calculateRating(float touchX) {
        float result = 0;
        for (int i = 0; i < mStarTotal - 1; i++) {
            if (touchX >= mBoundaryList.get(i) && touchX <= mBoundaryList.get(i + 1)) {
                if (mStarHalfDrawable != null && touchX < (mBoundaryList.get(i) + mBoundaryList.get(i + 1)) / 2)
                    result = i + 0.5f;
                else
                    result = i + 1;
                break;
            }
        }
        if (result == 0f && touchX >= mBoundaryList.get(mBoundaryList.size() - 1)) {
            result = touchX < mBoundaryList.get(mBoundaryList.size() - 1) + mStarWidth / 2 ? mStarTotal - 0.5f : mStarTotal;
        }
        if (result == 0f) {
            result = mStarHalfDrawable != null ? 0.5f : 1;
        }
        return result;
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (mRatingStatus == RatingStatus.Enable && !mBoundaryList.isEmpty()) {
            if (event.getAction() == MotionEvent.ACTION_DOWN) {
                float calculatedRating = calculateRating(event.getX());
                // 如果点击位置对应的评分与当前评分相同，则重置为 0
                if (Math.abs(calculatedRating - mRating) < 0.1f) {
                    setRating(0);
                } else {
                    setRating(calculatedRating);
                }
                return true;
            }
        }
        return super.onTouchEvent(event);
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        super.onLayout(changed, l, t, r, b);
        if (mBoundaryList.isEmpty()) {
            for (int index = 0; index < mStarTotal; index++) {
                mBoundaryList.add(index == 0 ? 0 : mBoundaryList.get(index - 1) + Math.round(mStarWidth) + Math.round(mStarPadding));
            }
        }
    }

    public void setOnRatingChangedListener(OnRatingChangedListener listener) {
        this.mOnRatingChangedListener = listener;
    }

    public void setRatingStatus(RatingStatus status) {
        this.mRatingStatus = status;
    }
}
