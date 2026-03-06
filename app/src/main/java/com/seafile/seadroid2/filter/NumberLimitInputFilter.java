package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.Spanned;

/**
 * @desc: Only numbers 、dots、comma are allowed
 */
public class NumberLimitInputFilter implements InputFilter {

    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        return null;
    }


}
