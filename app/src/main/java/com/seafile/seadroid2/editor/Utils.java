package com.seafile.seadroid2.editor;

import android.support.annotation.Nullable;
import android.text.style.AlignmentSpan;
import android.widget.EditText;

public class Utils {
    /**
     * find '\n' from "start" position
     *
     * @param s     text
     * @param start start position
     * @return the '\n' position
     */
    public static int findNextNewLineChar(CharSequence s, int start) {
        for (int i = start; i < s.length(); i++) {
            if (s.charAt(i) == '\n') {
                return i;
            }
        }
        return -1;
    }

    /**
     * find '\n' before "start" position
     *
     * @param s     text
     * @param start start position
     * @return the '\n' position
     */
    public static int findBeforeNewLineChar(CharSequence s, int start) {
        for (int i = start - 1; i > 0; i--) {
            if (s.charAt(i) == '\n') {
                return i;
            }
        }
        return -1;
    }

    @Nullable
    public static <T> T getSpans(EditText editText, int start, int end, Class<T> clazz) {
        T[] ts = editText.getText().getSpans(start, end, clazz);
        if (ts != null && ts.length > 0) {
            return ts[0];
        }
        return null;
    }

    public static boolean hasCenterSpan(EditText editText, int start, int end) {
        AlignmentSpan.Standard centerSpan = Utils.getSpans(editText, start, end, AlignmentSpan.Standard.class);
        if (centerSpan == null) {
            return false;
        } else {
            return true;
        }
    }


    public static int safePosition(int position, CharSequence s) {
        return position > s.length() ? s.length() : (position < 0 ? 0 : position);
    }

}
