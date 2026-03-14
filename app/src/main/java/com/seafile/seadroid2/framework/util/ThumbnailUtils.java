package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import com.blankj.utilcode.util.EncodeUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.framework.db.entities.DirentModel;

import org.apache.commons.lang3.StringUtils;

public class ThumbnailUtils {
    public static String convertThumbnailUrl(String serverUrl, DirentModel direntModel) {

        if (TextUtils.isEmpty(serverUrl)) {
            return null;
        }

        if (!StringUtils.isEmpty(direntModel.encoded_thumbnail_src)) {
            long sec = TimeUtils.getNowMills() / 1000;
            return Utils.pathJoin(serverUrl, direntModel.encoded_thumbnail_src) + "?mtime=" + sec;
        }

        return ThumbnailUtils.convertThumbnailUrl(serverUrl, direntModel.repo_id, direntModel.full_path);
    }

    public static String convertThumbnailUrl(String serverUrl, String repoId, String fullPath) {
        return convertThumbnailUrl(serverUrl, repoId, fullPath, 256);
    }

    public static String convertThumbnailUrl(String serverUrl, String repoId, String fullPath, int size) {
        String newFilePath = EncodeUtils.urlEncode(fullPath);
        return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%s", serverUrl, repoId, newFilePath, size);
    }
}
