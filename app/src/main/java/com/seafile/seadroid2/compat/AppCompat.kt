package com.seafile.seadroid2.compat

import android.content.Context
import android.content.res.Configuration
import androidx.appcompat.app.AppCompatDelegate

fun Context.systemInNightMode(): Int {
    return resources.configuration.uiMode
}

fun Context.isSystemInNightMode(): Boolean {
    val c = resources.configuration.uiMode and Configuration.UI_MODE_NIGHT_MASK
    return c == Configuration.UI_MODE_NIGHT_YES
}

fun Context.isAppInNightMode(): Boolean {
    val a = AppCompatDelegate.getDefaultNightMode()
    if (a == AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM) {
        return isSystemInNightMode()
    }
    return a == AppCompatDelegate.MODE_NIGHT_YES
}