package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.SpannableStringBuilder;
import android.text.Spanned;

public class NumberInputFilter implements InputFilter {
    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {

        SpannableStringBuilder modification = new SpannableStringBuilder();
        for (int i = start; i < end; i++) {
            char c = source.charAt(i);
            if (isNumber(c)) {
                modification.append(c);
            }
        }
        return modification;
    }

    public static boolean isNumber(char c) {
        return ('0' <= c && c <= '9');
    }
}
