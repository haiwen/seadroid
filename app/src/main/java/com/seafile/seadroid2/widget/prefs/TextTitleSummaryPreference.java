package com.seafile.seadroid2.widget.prefs;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.preference.PreferenceViewHolder;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.widget.prefs.background_pref.BackgroundShapePreference;

public class TextTitleSummaryPreference extends BackgroundShapePreference {
    public TextTitleSummaryPreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }

    public TextTitleSummaryPreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public TextTitleSummaryPreference(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public TextTitleSummaryPreference(@NonNull Context context) {
        super(context);
    }

    @Override
    public int getLayoutId() {
        return R.layout.layout_pref_title_summary;
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
