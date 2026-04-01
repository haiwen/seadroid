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

    private MaterialSwitch mMaterialSwitch;

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

    @Override
    public void onBindViewHolder(@NonNull PreferenceViewHolder holder) {
        super.onBindViewHolder(holder);

        mMaterialSwitch = (MaterialSwitch) holder.findViewById(android.R.id.switch_widget);
        // SwitchPreferenceCompat.syncSwitchView() does not reliably propagate
        // mChecked to a MaterialSwitch in a custom layout. Force it here.
        if (mMaterialSwitch != null) {
            mMaterialSwitch.setChecked(isChecked());
            mMaterialSwitch.setClickable(false);
        }
    }

    @Override
    public void setChecked(boolean checked) {
        super.setChecked(checked);

        if (mMaterialSwitch != null) {
            mMaterialSwitch.setChecked(checked);
        }
    }
}
