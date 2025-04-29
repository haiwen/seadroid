package com.seafile.seadroid2.ui.comparator;

import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;

import java.math.BigInteger;
import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

public class NaturalOrderComparator implements Comparator<Object> {
    private Collator collator;

    public NaturalOrderComparator() {
        Locale preferred = Locale.getDefault();
        try {
            collator = Collator.getInstance(preferred);
        } catch (Exception e) {
            collator = Collator.getInstance(Locale.ROOT);
        }
        this.collator.setStrength(Collator.SECONDARY); // 忽略大小写和重音
    }

    @Override
    public Comparator<Object> reversed() {
        return Comparator.super.reversed();
    }

    @Override
    public int compare(Object s1, Object s2) {
        if (s1 == null && s2 == null) return 0; // all null
        if (s1 == null) return -1;
        if (s2 == null) return 1;

        // 类型检查和名称提取
        String name1 = extractName(s1);
        String name2 = extractName(s2);

        if (name1 == null && name2 == null) return 0;
        if (name1 == null) return -1;
        if (name2 == null) return 1;

        if (isEmpty(name1) && isEmpty(name2)) return 0;
        if (isEmpty(name1)) return -1;
        if (isEmpty(name2)) return 1;

        return compareNatural(name1, name2);
    }

    public static boolean isEmpty(CharSequence str) {
        return str == null || str.length() == 0;
    }

    private int compareNatural(String name1, String name2) {
        int len1 = name1.length();
        int len2 = name2.length();
        int i1 = 0, i2 = 0;

        while (i1 < len1 && i2 < len2) {
            char c1 = name1.charAt(i1);
            char c2 = name2.charAt(i2);

            // If it's all numbers, the entire number part is extracted for comparison
            if (Character.isDigit(c1) && Character.isDigit(c2)) {
                BigInteger num1 = extractNumber(name1, i1);
                BigInteger num2 = extractNumber(name2, i2);
                int cmp = num1.compareTo(num2);
                if (cmp != 0) {
                    return cmp;
                }

                // Skip the part of the number that has been processed
                i1 = skipDigits(name1, i1);
                i2 = skipDigits(name2, i2);
                continue;
            }

            // **确保数字排在字母前**
            if (Character.isDigit(c1)) return -1;
            if (Character.isDigit(c2)) return 1;

            //
            int casePriority = compareCasePriority(c1, c2);
            if (casePriority != 0) {
                return casePriority;
            }

            // 使用 Collator 比较字符（忽略大小写和重音）
            int cmp = collator.compare(Character.toString(c1), Character.toString(c2));
            if (cmp != 0) {
                return cmp;
            }

            i1++;
            i2++;

        }

        // If the previous parts are all the same, compare by length
        return Integer.compare(len1, len2);
    }

    private int compareCasePriority(char c1, char c2) {
        boolean isLower1 = Character.isLowerCase(c1);
        boolean isLower2 = Character.isLowerCase(c2);

        // 小写优先逻辑
        if (isLower1 && !isLower2) return -1;
        if (!isLower1 && isLower2) return 1;

        // 同大小写状态时保持原始顺序
        return Character.compare(c1, c2);
    }

    private String extractName(Object obj) {
        if (obj instanceof RepoModel) return ((RepoModel) obj).repo_name;
        if (obj instanceof DirentModel) return ((DirentModel) obj).name;
        if (obj instanceof String) return (String) obj;
        throw new IllegalArgumentException("Unsupported type: " + obj.getClass().getName());
    }

    private BigInteger extractNumber(String s, int start) {
        int end = start;
        while (end < s.length() && Character.isDigit(s.charAt(end))) {
            end++;
        }
        return new BigInteger(s.substring(start, end));
    }

    private int skipDigits(String s, int start) {
        while (start < s.length() && Character.isDigit(s.charAt(start))) {
            start++;
        }
        return start;
    }
}
