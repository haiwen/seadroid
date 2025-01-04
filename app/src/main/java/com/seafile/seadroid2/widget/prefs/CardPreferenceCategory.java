package com.seafile.seadroid2.widget.prefs;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.content.ContextCompat;
import androidx.preference.PreferenceCategory;
import androidx.preference.PreferenceViewHolder;
import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.R;

public class CardPreferenceCategory extends PreferenceCategory {
    public CardPreferenceCategory(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init(context, attrs, defStyleAttr);
    }

    public CardPreferenceCategory(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context, attrs, defStyleAttr);
    }

    public CardPreferenceCategory(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs, 0);
    }

    public CardPreferenceCategory(@NonNull Context context) {
        super(context);
        init(context, null, 0);
    }

//    private int marginBottom = 0;

    private void init(Context context, AttributeSet attrs, int defStyleAttr) {
//        if (context != null && attrs != null) {
//            TypedArray typedArray = context.obtainStyledAttributes(attrs,
//                    R.styleable.PrefCategoryShape, defStyleAttr, 0);
//
//            marginBottom = typedArray.getDimensionPixelSize(R.styleable.PrefCategoryShape_marginBottom, 0);
//
//            typedArray.recycle();
//        }


        setLayoutResource(R.layout.layout_pref_category);
    }

    @Override
    public void onBindViewHolder(@NonNull PreferenceViewHolder holder) {
        super.onBindViewHolder(holder);

        TextView titleView = (TextView) holder.findViewById(android.R.id.title);
        titleView.setText(getTitle());
    }

}
