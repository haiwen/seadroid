package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.Spanned;

/**
 * @desc: Carriage returns, spaces are prohibited
 */
public class NoSpaceEnterInputFilter implements InputFilter {
    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        //no empty space
        if (source.toString().equals(" ")) {
            return "";
        }
        if (source.toString().equals("\n")) {
            return "";
        }
        return null;
    }
}
