package com.seafile.seadroid2.ui;

import android.annotation.TargetApi;
import android.content.Context;
import android.os.Build;
import android.preference.CheckBoxPreference;
import android.util.AttributeSet;

import com.seafile.seadroid2.R;

/**
 * SwitchCompat is a version of the Switch widget which on devices back to API v7.
 * It does not make any attempt to use the platform provided widget on those devices which it is available normally.
 *
 * A Switch is a two-state toggle switch widget that can select between two options.
 * The user may drag the "thumb" back and forth to choose the selected option, or simply tap to toggle as if it were a checkbox.
 */
public class SwitchPreferenceCompat extends CheckBoxPreference {
    public SwitchPreferenceCompat(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        setLayoutRes();
    }

    @TargetApi(Build.VERSION_CODES.LOLLIPOP)
    public SwitchPreferenceCompat(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        setLayoutRes();
    }

    public SwitchPreferenceCompat(Context context, AttributeSet attrs) {
        super(context, attrs);
        setLayoutRes();
    }

    public SwitchPreferenceCompat(Context context) {
        super(context);
        setLayoutRes();
    }

    private void setLayoutRes() {
        setWidgetLayoutResource(R.layout.switch_compat_preference);
    }
}
