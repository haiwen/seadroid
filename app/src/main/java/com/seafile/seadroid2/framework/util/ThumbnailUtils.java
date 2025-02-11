package com.seafile.seadroid2.framework.util;

import com.blankj.utilcode.util.EncodeUtils;

public class ThumbnailUtils {

    public static String convertThumbnailUrl(String serverUrl, String repoId, String fullPath) {
        return convertThumbnailUrl(serverUrl, repoId, fullPath, 512);
    }

    public static String convertThumbnailUrl(String serverUrl, String repoId, String fullPath, int size) {
        String newFilePath = EncodeUtils.urlEncode(fullPath);
        return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%s", serverUrl, repoId, newFilePath, size);
    }
}
