package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.Spanned;

/**
 * The specified character cannot be repeated
 */
public class CharacterNoRepeatSpecialCountInputFilter implements InputFilter {
    char character;
    int maxCount = 1;

    public CharacterNoRepeatSpecialCountInputFilter(char character, int maxCount) {
        this.character = character;
        this.maxCount = maxCount;
        if (character == 0) {
            throw new IllegalArgumentException("character can not be empty");
        }
    }

    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        //source : old input
        //dest : new input

        //empty
        if (source == null || source.length() == 0) {
            if (dest == null) {
                return null;
            } else if (dest.length() == 0) {
                return null;
            } else {
                long count = dest.chars().filter(ch -> ch == character).count();
                if (count > maxCount) {
                    return "";
                } else {
                    return null;
                }
            }
        } else {
            long oldCount = source.chars().filter(ch -> ch == character).count();
            if (dest == null) {
                return null;
            } else if (dest.length() == 0) {
                return null;
            } else {
                long count = dest.chars().filter(ch -> ch == character).count();
                if ((oldCount + count) > maxCount) {
                    return "";
                } else {
                    return null;
                }
            }
        }
    }
}
