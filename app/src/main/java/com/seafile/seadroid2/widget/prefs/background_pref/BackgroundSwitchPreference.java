package com.seafile.seadroid2.widget.prefs.background_pref;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.preference.PreferenceViewHolder;
import androidx.preference.SwitchPreferenceCompat;

import com.google.android.material.divider.MaterialDivider;
import com.seafile.seadroid2.R;

public abstract class BackgroundSwitchPreference extends SwitchPreferenceCompat {
    public BackgroundSwitchPreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init(context, attrs, defStyleAttr);
    }

    public BackgroundSwitchPreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context, attrs, defStyleAttr);
    }

    public BackgroundSwitchPreference(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs, 0);
    }

    public BackgroundSwitchPreference(@NonNull Context context) {
        super(context);
        init(context, null, 0);
    }

    //
    public abstract int getLayoutId();

    private int titleTextColor = 0;
    //default all
    private int radiusPosition = 0;
    private int backgroundColor = 0;
    private int backgroundRadius = 0;
    private int dividerPosition = 0;

    private void init(Context context, AttributeSet attrs, int defStyleAttr) {

        if (context != null && attrs != null) {
            TypedArray typedArray = context.obtainStyledAttributes(attrs,
                    R.styleable.PrefShape, defStyleAttr, 0);

            titleTextColor = typedArray.getColor(R.styleable.PrefShape_titleTextColor, ContextCompat.getColor(getContext(), R.color.item_title_color));
            radiusPosition = typedArray.getInt(R.styleable.PrefShape_radiusPosition, 0);

            backgroundColor = typedArray.getColor(R.styleable.PrefShape_backgroundColor, ContextCompat.getColor(getContext(), R.color.bar_background_color));
            backgroundRadius = typedArray.getDimensionPixelSize(R.styleable.PrefShape_backgroundRadius, 8);

            dividerPosition = typedArray.getInt(R.styleable.PrefShape_dividerPosition, 0);

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

        MaterialDivider topDivider = (MaterialDivider) holder.findViewById(R.id.top_divider);
        MaterialDivider bottomDivider = (MaterialDivider) holder.findViewById(R.id.bottom_divider);
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

    /**
     * dp
     */
    public void setBackgroundRadius(int backgroundRadius) {
        this.backgroundRadius = backgroundRadius;
        notifyChanged();
    }

    /**
     * @param radiusPosition 0: none, 1: all, 2: top, 3: bottom
     */
    public void setRadiusPosition(int radiusPosition) {
        this.radiusPosition = radiusPosition;
        notifyChanged();
    }

    /**
     * @param dividerPosition 0: none, 1: top, 2: bottom, 3: top and bottom
     */
    public void setDividerPosition(int dividerPosition) {
        this.dividerPosition = dividerPosition;
        notifyChanged();
    }

    public void setTitleTextColor(int titleTextColor) {
        this.titleTextColor = titleTextColor;
        notifyChanged();
    }
}
