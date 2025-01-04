package com.seafile.seadroid2.widget.prefs;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.preference.PreferenceViewHolder;

import com.google.android.material.materialswitch.MaterialSwitch;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.widget.prefs.background_pref.BackgroundSwitchPreference;

public class TextSwitchPreference extends BackgroundSwitchPreference {
    public TextSwitchPreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }

    public TextSwitchPreference(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public TextSwitchPreference(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public TextSwitchPreference(@NonNull Context context) {
        super(context);
    }

    @Override
    public int getLayoutId() {
        return R.layout.layout_pref_title_switch;
    }

    private MaterialSwitch materialSwitch;

    @Override
    public void onBindViewHolder(@NonNull PreferenceViewHolder holder) {
        super.onBindViewHolder(holder);

        materialSwitch = (MaterialSwitch) holder.findViewById(android.R.id.switch_widget);
        materialSwitch.setClickable(false);
    }

    public void setChecked(boolean checked) {
        super.setChecked(checked);

        if (materialSwitch != null) {
            materialSwitch.setChecked(checked);
        }
    }
}
