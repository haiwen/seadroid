package com.seafile.seadroid2.filter;

import android.text.InputType;
import android.text.method.NumberKeyListener;

import androidx.annotation.NonNull;

/**
 * @desc: Only numbers 、dots、comma are allowed
 */
public class NumberDotOnlyInputFilter extends NumberKeyListener {
    @Override
    public int getInputType() {
        return InputType.TYPE_CLASS_NUMBER;
    }

    @NonNull
    @Override
    protected char[] getAcceptedChars() {
        return "0123456789.".toCharArray();
    }
}
