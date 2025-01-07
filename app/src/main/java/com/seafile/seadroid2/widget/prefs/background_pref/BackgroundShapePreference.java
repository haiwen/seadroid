package com.seafile.seadroid2.widget.prefs.background_pref;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.LayerDrawable;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.preference.Preference;
import androidx.preference.PreferenceViewHolder;

import com.google.android.material.divider.MaterialDivider;
import com.seafile.seadroid2.R;

public abstract class BackgroundShapePreference extends Preference {

    public BackgroundShapePreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init(context, attrs, defStyleAttr);
    }

    public BackgroundShapePreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context, attrs, defStyleAttr);
    }

    public BackgroundShapePreference(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs, 0);
    }

    public BackgroundShapePreference(@NonNull Context context) {
        super(context);
        init(context, null, 0);
    }

    //
    public abstract int getLayoutId();

    private int titleTextColor = 0;
    private int summaryTextColor = 0;
    //default all
    private int radiusPosition = 0;
    private int backgroundColor = 0;
    private int backgroundRadius = 0;
    private int dividerPosition = 0;
    private int dividerColor = 0;

    private void init(Context context, AttributeSet attrs, int defStyleAttr) {

        if (context != null && attrs != null) {
            TypedArray typedArray = context.obtainStyledAttributes(attrs,
                    R.styleable.PrefShape, defStyleAttr, 0);

            titleTextColor = typedArray.getColor(R.styleable.PrefShape_titleTextColor, ContextCompat.getColor(getContext(), R.color.item_title_color));
            summaryTextColor = typedArray.getColor(R.styleable.PrefShape_summaryTextColor, ContextCompat.getColor(getContext(), R.color.item_subtitle_color));
            radiusPosition = typedArray.getInt(R.styleable.PrefShape_radiusPosition, 0);

            backgroundColor = typedArray.getColor(R.styleable.PrefShape_backgroundColor, ContextCompat.getColor(getContext(), R.color.bar_background_color));
            backgroundRadius = typedArray.getDimensionPixelSize(R.styleable.PrefShape_backgroundRadius, 8);

            dividerPosition = typedArray.getInt(R.styleable.PrefShape_dividerPosition, 0);
            dividerColor = typedArray.getColor(R.styleable.PrefShape_dividerColor, ContextCompat.getColor(getContext(), R.color.divider_color));

            typedArray.recycle();
        }

        setLayoutResource(getLayoutId());
    }

    @Override
    public void onBindViewHolder(@NonNull PreferenceViewHolder holder) {
        super.onBindViewHolder(holder);

        Drawable drawable = BackgroundShapeUtils.genBackgroundDrawable(radiusPosition, backgroundColor, backgroundRadius);
        holder.itemView.setBackground(drawable);

        TextView titleTextView = (TextView) holder.findViewById(android.R.id.title);
        if (titleTextView != null) {
            titleTextView.setTextColor(titleTextColor);
        }

        TextView summaryTextView = (TextView) holder.findViewById(android.R.id.summary);
        if (summaryTextView != null) {
            summaryTextView.setTextColor(summaryTextColor);
        }


        MaterialDivider topDivider = (MaterialDivider) holder.findViewById(R.id.top_divider);
        MaterialDivider bottomDivider = (MaterialDivider) holder.findViewById(R.id.bottom_divider);
        topDivider.setDividerColor(dividerColor);
        bottomDivider.setDividerColor(dividerColor);

        if (dividerPosition == 0) {
            //none
            topDivider.setVisibility(View.GONE);
            bottomDivider.setVisibility(View.GONE);
        } else if (dividerPosition == 1) {
            //top
            topDivider.setVisibility(View.VISIBLE);
            bottomDivider.setVisibility(View.GONE);
        } else if (dividerPosition == 2) {
            //bottom
            topDivider.setVisibility(View.GONE);
            bottomDivider.setVisibility(View.VISIBLE);
        } else if (dividerPosition == 3) {
            //top and bottom
            topDivider.setVisibility(View.VISIBLE);
            bottomDivider.setVisibility(View.VISIBLE);
        }
    }

    public void setBackgroundColor(int backgroundColor) {
        this.backgroundColor = backgroundColor;
        notifyChanged();
    }

    public void setBackgroundRadius(int backgroundRadius) {
        this.backgroundRadius = backgroundRadius;
        notifyChanged();
    }

    public void setRadiusPosition(int radiusPosition) {
        this.radiusPosition = radiusPosition;
        notifyChanged();
    }

    public void setDividerPosition(int dividerPosition) {
        this.dividerPosition = dividerPosition;
        notifyChanged();
    }

    public void setTitleTextColor(int titleTextColor) {
        this.titleTextColor = titleTextColor;
        notifyChanged();
    }
}
