package com.seafile.seadroid2.framework.util;

import org.apache.commons.lang3.StringUtils;

import java.text.Normalizer;

public class UnicodePathUtils {
    private UnicodePathUtils() {
    }

    public static String normalize(String s) {
        if (StringUtils.isEmpty(s)) {
            return null;
        }
        return Normalizer.normalize(s, Normalizer.Form.NFC);
    }
}
