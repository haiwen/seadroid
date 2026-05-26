package com.seafile.seadroid2.framework.util;

import java.text.Normalizer;

public class UnicodePathUtils {
    private UnicodePathUtils() {
    }

    public static String normalize(String s) {
        if (s == null) {
            return null;
        }
        return Normalizer.normalize(s, Normalizer.Form.NFC);
    }
}
