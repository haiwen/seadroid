package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import com.blankj.utilcode.util.EncodeUtils;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.ServerInfo;

public class ThumbnailUtils {
    private static final int DEFAULT_THUMBNAIL_SIZE = 256;
    private static final String NEW_THUMBNAIL_API_VERSION = "14.0.0";

    public static String convertThumbnailUrl(String serverUrl, DirentModel model) {
        if (model == null) {
            return null;
        }
        return convertThumbnailUrl(serverUrl, model.repo_id, model.getFullName());
    }

    /**
     * <p> < 14.0 : api2/repos/{repo_id}/thumbnail/ </p>
     * <p> >=14.0 : thumbnail/{repo_id}/{size}/{full_path} </p>
     */
    public static String convertThumbnailUrl(String serverUrl, String repoId, String fullPathName) {
        if (TextUtils.isEmpty(serverUrl) || TextUtils.isEmpty(repoId) || TextUtils.isEmpty(fullPathName)) {
            return null;
        }

        if (!serverUrl.endsWith("/")) {
            serverUrl = serverUrl + "/";
        }

        String encodedPath = EncodeUtils.urlEncode(fullPathName);
        return supportsNewThumbnailApi()
                ? getNewThumbnailUrl(serverUrl, repoId, encodedPath)
                : getOldThumbnailUrl(serverUrl, repoId, encodedPath);
    }

    private static boolean supportsNewThumbnailApi() {
        ServerInfo serverInfo = SupportAccountManager.getInstance().getCurrentServerInfo();
        return serverInfo != null && Utils.compareVersion(serverInfo.getVersion(), NEW_THUMBNAIL_API_VERSION) >= 0;
    }

    private static String getOldThumbnailUrl(String serverUrl, String repoId, String encodedName) {
        return serverUrl + "api2/repos/" + repoId + "/thumbnail/?p=" + encodedName + "&size=" + ThumbnailUtils.DEFAULT_THUMBNAIL_SIZE;
    }

    private static String getNewThumbnailUrl(String serverUrl, String repoId, String encodedPath) {
        return serverUrl + "thumbnail/" + repoId + "/" + ThumbnailUtils.DEFAULT_THUMBNAIL_SIZE + "/" + encodedPath;
    }
}
