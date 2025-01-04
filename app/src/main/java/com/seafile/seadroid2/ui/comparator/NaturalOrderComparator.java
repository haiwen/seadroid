package com.seafile.seadroid2.ui.comparator;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Comparator;

public class NaturalOrderComparator implements Comparator<Object> {

    @Override
    public Comparator<Object> reversed() {
        return Comparator.super.reversed();
    }

    @Override
    public int compare(Object s1, Object s2) {
        String name1 = "";
        String name2 = "";
        if (s1 instanceof RepoModel m1 && s2 instanceof RepoModel m2) {
            name1 = m1.repo_name;
            name2 = m2.repo_name;
        } else if (s1 instanceof DirentModel m1 && s2 instanceof DirentModel m2) {
            name1 = m1.name;
            name2 = m2.name;
        }

        if (TextUtils.isEmpty(name1) || TextUtils.isEmpty(name2)) {
            return -1;
        }

        int len1 = name1.length();
        int len2 = name2.length();
        int i1 = 0, i2 = 0;

        while (i1 < len1 && i2 < len2) {
            char c1 = name1.charAt(i1);
            char c2 = name2.charAt(i2);

            // If it's all numbers, the entire number part is extracted for comparison
            if (Character.isDigit(c1) && Character.isDigit(c2)) {
                BigDecimal num1 = extractNumber(name1, i1);
                BigDecimal num2 = extractNumber(name2, i2);
                int cmp = num1.compareTo(num2);
                if (cmp != 0) {
                    return cmp;
                }

                // Skip the part of the number that has been processed
                i1 = skipDigits(name1, i1);
                i2 = skipDigits(name2, i2);
            } else {
                // 否则按字符比较
                if (c1 != c2) {

                    // If c1 is lowercase and c2 is uppercase, c1 should come first
                    if (Character.isLowerCase(c1) && Character.isUpperCase(c2)) {
                        return -1;
                    }
                    // If c1 is uppercase and c2 is lowercase, c2 should come first
                    if (Character.isUpperCase(c1) && Character.isLowerCase(c2)) {
                        return 1;
                    }

                    return c1 - c2;
                }
                i1++;
                i2++;
            }
        }

        // If the previous parts are all the same, compare by length
        return len1 - len2;
    }

    private BigDecimal extractNumber(String s, int start) {
        int end = start;
        while (end < s.length() && Character.isDigit(s.charAt(end))) {
            end++;
        }
        return new BigDecimal(s.substring(start, end));
    }

    private int skipDigits(String s, int start) {
        while (start < s.length() && Character.isDigit(s.charAt(start))) {
            start++;
        }
        return start;
    }

    @Override
    public boolean equals(Object obj) {
        return false;
    }
}
