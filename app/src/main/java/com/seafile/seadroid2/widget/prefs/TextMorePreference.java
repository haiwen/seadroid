package com.seafile.seadroid2.widget.prefs;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.preference.Preference;
import androidx.preference.PreferenceViewHolder;

import com.seafile.seadroid2.R;

public class TextMorePreference extends BackgroundShapePreference {
    public TextMorePreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }

    public TextMorePreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public TextMorePreference(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public TextMorePreference(@NonNull Context context) {
        super(context);
    }

    @Override
    public int getLayoutId() {
        return R.layout.layout_pref_text_and_more;
    }

    public TextView getTitleTextView() {
        return titleTextView;
    }

    TextView titleTextView;

    @Override
    protected void onClick() {
        super.onClick();
    }

    @Override
    public void onBindViewHolder(@NonNull PreferenceViewHolder holder) {
        super.onBindViewHolder(holder);

        titleTextView = (TextView) holder.findViewById(android.R.id.title);
        titleTextView.setText(getTitle());
    }
}
