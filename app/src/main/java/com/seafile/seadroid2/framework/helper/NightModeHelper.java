package com.seafile.seadroid2.framework.helper;

import androidx.appcompat.app.AppCompatDelegate;

import com.seafile.seadroid2.enums.NightMode;
import com.seafile.seadroid2.preferences.Settings;

public class NightModeHelper {

    public static void syncTo(NightMode nightMode) {
        AppCompatDelegate.setDefaultNightMode(nightMode.ordinal());
        //save into sharedPreferences
        Settings.NIGHT_MODE.putValue(nightMode);
    }
}
