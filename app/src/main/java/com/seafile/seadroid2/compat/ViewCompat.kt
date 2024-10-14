package com.seafile.seadroid2.compat

import android.content.res.ColorStateList
import android.graphics.PorterDuff
import android.graphics.drawable.Drawable
import android.view.View
import androidx.annotation.IdRes
import androidx.core.view.ViewCompat
import com.seafile.seadroid2.framework.util.ForegroundCompat

@Suppress("UNCHECKED_CAST")
fun <T : View> View.requireViewByIdCompat(@IdRes id: Int): T = ViewCompat.requireViewById<T>(this, id)

var View.scrollIndicatorsCompat: Int
    get() = ViewCompat.getScrollIndicators(this)
    set(value) {
        ViewCompat.setScrollIndicators(this, value)
    }

fun View.setScrollIndicatorsCompat(indicators: Int, mask: Int) {
    ViewCompat.setScrollIndicators(this, indicators, mask)
}

var View.foregroundCompat: Drawable?
    get() = ForegroundCompat.getForeground(this)
    set(value) {
        ForegroundCompat.setForeground(this, value)
    }

var View.foregroundGravityCompat: Int
    get() = ForegroundCompat.getForegroundGravity(this)
    set(value) {
        ForegroundCompat.setForegroundGravity(this, value)
    }

var View.foregroundTintListCompat: ColorStateList?
    get() = ForegroundCompat.getForegroundTintList(this)
    set(value) {
        ForegroundCompat.setForegroundTintList(this, value)
    }

var View.foregroundTintModeCompat: PorterDuff.Mode?
    get() = ForegroundCompat.getForegroundTintMode(this)
    set(value) {
        ForegroundCompat.setForegroundTintMode(this, value)
    }
