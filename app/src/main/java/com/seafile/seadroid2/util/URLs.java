package com.seafile.seadroid2.util;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.CollectionUtils;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class URLs {

    /**
     * Gets the original file name
     *
     * @return logo.png@100px|100 if url is xxx/xxx/logo.png@100px|100
     */
    @Nullable
    public static String getOriginalFileName(String url) {
        if (TextUtils.isEmpty(url)) {
            return null;
        }

        URL url1 = null;
        try {
            url1 = new URL(url);
        } catch (MalformedURLException e) {
            return null;
        }

        String path = url1.getPath();
        if (TextUtils.isEmpty(path)) {
            return null;
        }

        path = StringUtils.trimEnd(path, "/");
        if (TextUtils.isEmpty(path)) {
            return null;
        }

        return new File(url1.getPath()).getName();
    }

    /**
     * @return logo.png if url is xxx/xxx/logo.png@100px|100
     */
    public static String getPerfectFileName(String url) throws MalformedURLException, UnsupportedEncodingException {
        if (TextUtils.isEmpty(url)) {
            return null;
        }
        String fileName = getOriginalFileName(url);
        if (TextUtils.isEmpty(fileName)) {
            return null;
        }

        return getNameAndFormat(fileName);
    }

    /**
     * @return png if url is xxx/xxx/logo.png@100px|100
     */
    public static String getFileNameFormat(String url) {
        if (TextUtils.isEmpty(url)) {
            return null;
        }

        String fileName = getOriginalFileName(url);
        if (TextUtils.isEmpty(fileName)) {
            return null;
        }

        return getFormat(fileName);
    }

    private static String getFormat(String fileName) {
        int dotIndex = fileName.lastIndexOf(".");
        if (dotIndex == -1) {
            return fileName;
        }

        String regex = "[^a-zA-Z0-9]"; // Whether it contains non-alphabetic-number characters
        String f = fileName.substring(dotIndex + 1);
        //if fileName = logo.png@100px|auto
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(f);
        if (matcher.find()) {
            int startIndex = matcher.start();
            if (startIndex == 0) {//not support if fileName = logo.@100px|auto
                return fileName;
            }

            return f.substring(0, startIndex);
        }
        return f;//return png, if fileName = logo.png
    }

    private static String getNameAndFormat(String fileName) {
        int dotIndex = fileName.lastIndexOf(".");
        if (dotIndex == -1) {
            return fileName;
        }

        String name = fileName.substring(0, dotIndex);
        String format = getFormat(fileName);
        return name + "." + format;
    }

    /**
     * https://dev.xxx.com/dev/ => dev.xxx.com
     */
    @Nullable
    public static String getHost(String url) {
        if (TextUtils.isEmpty(url)) {
            return null;
        }
        try {
            URL url1 = new URL(url);
            return url1.getHost();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * https://dev.xxx.com/dev/ => https://dev.xxx.com
     */
    @Nullable
    public static String getProtocolHost(String url) {
        if (TextUtils.isEmpty(url)) {
            return null;
        }
        try {
            URL url1 = new URL(url);
            return url1.getProtocol() + "://" + url1.getHost();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * https://dev.xxx.com/dev/?next=/dev/test.md => next=/dev/test.md
     */
    @Nullable
    public static String getQuery(String url) {
        if (TextUtils.isEmpty(url)) {
            return null;
        }
        try {
            URL url1 = new URL(url);
            return url1.getQuery();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * https://dev.xxx.com/dev/?page=1&size=10
     * =>
     * ["page=1","size=10"]
     */
    @Nullable
    public static List<String> getQueries(String url) {
        if (TextUtils.isEmpty(url)) {
            return null;
        }

        try {
            URL url1 = new URL(url);
            String q = url1.getQuery();
            if (TextUtils.isEmpty(q)) {
                return null;
            }

            if (!q.contains("&")) {
                return CollectionUtils.newArrayList(q);
            }

            String[] sq = q.split("&");
            return Arrays.asList(sq);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
}
