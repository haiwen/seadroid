package com.seafile.seadroid2.enums;

import androidx.appcompat.app.AppCompatDelegate;

public enum NightMode {
    FOLLOW_SYSTEM(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM),
    OFF(AppCompatDelegate.MODE_NIGHT_NO),
    ON(AppCompatDelegate.MODE_NIGHT_YES);

    NightMode(int i) {

    }
}
