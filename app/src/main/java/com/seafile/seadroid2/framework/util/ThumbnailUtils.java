package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.EncodeUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.ServerInfo;

import org.apache.commons.lang3.StringUtils;

import java.util.Locale;

public class ThumbnailUtils {

    public static String convertThumbnailUrl(String serverUrl, DirentModel model) {
        return convertThumbnailUrl(serverUrl, model.repo_id, 256, model.getName());
    }

    /**
     * <p> < 14.0 : /api2/repos/{repo_id}/thumbnail/ </p>
     * <p> >=14.0 : (thumbnail/:repo_id/:size/:path </p>
     *
     */
    public static String convertThumbnailUrl(String serverUrl, String repoId, int thumbSize, String fileName) {
        if (TextUtils.isEmpty(serverUrl)) {
            return null;
        }

        String newFile = EncodeUtils.urlEncode(fileName);

        ServerInfo serverInfo = SupportAccountManager.getInstance().getCurrentServerInfo();

        if (serverInfo == null) {
            return getOldThumbnailUrl(serverUrl, repoId, thumbSize, newFile);
        }

        int r = Utils.compareVersion(serverInfo.getVersion(), "14.0.0");
        if (r >= 0) {
            return getNewThumbnailUrl(serverUrl, repoId, thumbSize, newFile);
        } else {
            return getOldThumbnailUrl(serverUrl, repoId, thumbSize, newFile);
        }
    }

    @NonNull
    private static String getOldThumbnailUrl(String serverUrl, String repoId, int thumbSize, String newFile) {
        return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%s", serverUrl, repoId, newFile, thumbSize);
    }

    @NonNull
    private static String getNewThumbnailUrl(String serverUrl, String repoId, int thumbSize, String newFile) {
        return String.format(Locale.getDefault(), "%sthumbnail/%s/%d/%s", serverUrl, repoId, thumbSize, newFile);
//        return Utils.pathJoin(serverUrl, "thumbnail", repoId, thumbSize + "", newFile);
    }
}
