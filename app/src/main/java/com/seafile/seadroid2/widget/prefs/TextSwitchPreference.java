package com.seafile.seadroid2.widget.prefs;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.preference.PreferenceViewHolder;

import com.google.android.material.materialswitch.MaterialSwitch;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.widget.prefs.background_pref.BackgroundSwitchPreference;

public class TextSwitchPreference extends BackgroundSwitchPreference {

    private MaterialSwitch mMaterialSwitch;
    private TextView mSwitchWidgetDesc;

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
        mSwitchWidgetDesc = (TextView) holder.findViewById(R.id.switch_widget_desc);

        // SwitchPreferenceCompat.syncSwitchView() does not reliably propagate
        // mChecked to a MaterialSwitch in a custom layout. Force it here.
        if (mMaterialSwitch != null) {
            mMaterialSwitch.setChecked(isChecked());
            mMaterialSwitch.setClickable(false);
        }
    }

    public void setSwitchDesc(String descStr) {
        if (mSwitchWidgetDesc != null) {
            mSwitchWidgetDesc.setText(descStr);
        }
    }

    public void setSwitchDesc(@StringRes int descStr) {
        if (mSwitchWidgetDesc != null) {
            mSwitchWidgetDesc.setText(descStr);
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
