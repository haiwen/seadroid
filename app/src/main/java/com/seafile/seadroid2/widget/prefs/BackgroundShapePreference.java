package com.seafile.seadroid2.widget.prefs;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.preference.Preference;
import androidx.preference.PreferenceViewHolder;

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

    //default all
    private int rShape = 0;

    private void init(Context context, AttributeSet attrs, int defStyleAttr) {

        if (context != null && attrs != null) {
            TypedArray typedArray = context.obtainStyledAttributes(attrs,
                    R.styleable.PrefBackgroundRadiusShapeEnum, defStyleAttr, 0);

            rShape = typedArray.getInt(R.styleable.PrefBackgroundRadiusShapeEnum_radiusShape, 0);
            typedArray.recycle();
        }

        setLayoutResource(getLayoutId());
    }

    @Override
    protected void onClick() {
        super.onClick();
    }

    @Override
    public void onBindViewHolder(@NonNull PreferenceViewHolder holder) {
        super.onBindViewHolder(holder);

        int rRes = switch (rShape) {
            case 0 ->//all radius, no divider
                    R.drawable.shape_radius_8;
            case 1 ->//none radius, top and bottom divider
                    R.drawable.shape_radius_0_divider;
            case 2 ->//top radius, no divider
                    R.drawable.shape_radius_8_top_left_right;
            case 3 ->//bottom radius, no divider
                    R.drawable.shape_radius_8_bottom_left_right;
            default -> 0;
        };

        if (rRes == 0) {
            rRes = R.drawable.shape_radius_8;
        }
        holder.itemView.setBackgroundResource(rRes);
    }
}
