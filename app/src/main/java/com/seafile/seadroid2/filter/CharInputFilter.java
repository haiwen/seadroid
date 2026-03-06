package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.SpannableStringBuilder;
import android.text.Spanned;


/**
 * {@link <a href="https://www.freesion.com/article/7867124120/">https://www.freesion.com/article/7867124120/</a>}
 */
public class CharInputFilter implements InputFilter {

    private int filterModel = 0xFF;

    public static final int MODEL_CHINESE = 1;

    public static final int MODEL_CHAR_LETTER = 2;

    public static final int MODEL_NUMBER = 4;

    public static final int MODEL_ASCII_CHAR = 8;

    private int maxInputLength = -1;


    public CharInputFilter() {
    }

    public CharInputFilter(int filterModel) {
        this.filterModel = filterModel;
    }

    public void setFilterModel(int filterModel) {
        this.filterModel = filterModel;
    }

    public void setMaxInputLength(int maxInputLength) {
        this.maxInputLength = maxInputLength;
    }

    public static boolean isChinese(char c) {
        Character.UnicodeBlock ub = Character.UnicodeBlock.of(c);
        if (ub == Character.UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS
                || ub == Character.UnicodeBlock.CJK_COMPATIBILITY_IDEOGRAPHS
                || ub == Character.UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A
                || ub == Character.UnicodeBlock.GENERAL_PUNCTUATION
                || ub == Character.UnicodeBlock.CJK_SYMBOLS_AND_PUNCTUATION
                || ub == Character.UnicodeBlock.HALFWIDTH_AND_FULLWIDTH_FORMS) {
            return true;
        }
        return false;
    }

    public static boolean isCharLetter(char c) {
        // Allow [a-zA-Z]
        if ('a' <= c && c <= 'z')
            return true;
        if ('A' <= c && c <= 'Z')
            return true;
        return false;
    }

    public static boolean isNumber(char c) {
        return ('0' <= c && c <= '9');
    }

    public static boolean isAsciiChar(char c) {
        return (33 <= c && c <= 126);
    }

    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        int length = dest.length() - (dend - dstart);
        if (maxInputLength > 0) {
            if (length == maxInputLength) {
                return "";
            }
        }

        SpannableStringBuilder modification = new SpannableStringBuilder();

        for (int i = start; i < end; i++) {
            char c = source.charAt(i);

            boolean append = false;

            if ((filterModel & MODEL_CHINESE) == MODEL_CHINESE) {
                append = isChinese(c) || append;
            }
            if ((filterModel & MODEL_CHAR_LETTER) == MODEL_CHAR_LETTER) {
                append = isCharLetter(c) || append;
            }
            if ((filterModel & MODEL_NUMBER) == MODEL_NUMBER) {
                append = isNumber(c) || append;
            }
            if ((filterModel & MODEL_ASCII_CHAR) == MODEL_ASCII_CHAR) {
                append = isAsciiChar(c) || append;
            }

            if (append) {
                modification.append(c);
            }
        }

        if (maxInputLength > 0) {

            int newLength = length + modification.length();
            if (newLength > maxInputLength) {
                modification.delete(maxInputLength - length, modification.length());
            }
        }

        return modification;
    }
}
