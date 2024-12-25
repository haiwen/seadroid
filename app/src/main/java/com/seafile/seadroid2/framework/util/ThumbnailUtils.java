package com.seafile.seadroid2.framework.util;

import com.blankj.utilcode.util.EncodeUtils;

public class ThumbnailUtils {

    public static String convertThumbnailUrl(String serverUrl, String repoId, String fullPath) {
        String newFilePath = EncodeUtils.urlEncode(fullPath);
        return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%s", serverUrl, repoId, newFilePath, 128);
    }
}
