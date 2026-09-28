package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;
import android.net.Uri;

import com.blankj.utilcode.util.EncodeUtils;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.model.ServerInfo;
import okhttp3.HttpUrl;

public class ThumbnailUtils {
    private static final int DEFAULT_THUMBNAIL_SIZE = 256;
    private static final String NEW_THUMBNAIL_API_VERSION = "13.0.0";

    public static String convertThumbnailUrl(String serverUrl, DirentModel model) {
        if (model == null) {
            return null;
        }
        String providedUrl = resolveThumbnailUrl(serverUrl, model.encoded_thumbnail_src);
        if (providedUrl != null) {
            return providedUrl;
        }
        return convertThumbnailUrl(serverUrl, model.repo_id, model.getFullName());
    }

    static String resolveThumbnailUrl(String serverUrl, String thumbnailSrc) {
        if (TextUtils.isEmpty(serverUrl) || TextUtils.isEmpty(thumbnailSrc)) {
            return null;
        }
        HttpUrl base = HttpUrl.parse(serverUrl.endsWith("/") ? serverUrl : serverUrl + "/");
        HttpUrl resolved = base == null ? null : base.resolve(thumbnailSrc);
        // Requests carry account credentials; only accept this server's origin.
        if (resolved == null || !base.scheme().equals(resolved.scheme())
                || !base.host().equals(resolved.host()) || base.port() != resolved.port()) {
            return null;
        }
        return resolved.toString();
    }

    /**
     * <p> < 13.0 : api2/repos/{repo_id}/thumbnail/ </p>
     * <p> >=13.0 : thumbnail/{repo_id}/{size}/{full_path} </p>
     */
    public static String convertThumbnailUrl(String serverUrl, String repoId, String fullPathName) {
        if (TextUtils.isEmpty(serverUrl) || TextUtils.isEmpty(repoId) || TextUtils.isEmpty(fullPathName)) {
            return null;
        }

        if (!serverUrl.endsWith("/")) {
            serverUrl = serverUrl + "/";
        }

        return supportsNewThumbnailApi()
                ? getNewThumbnailUrl(serverUrl, repoId, fullPathName)
                : getOldThumbnailUrl(serverUrl, repoId, EncodeUtils.urlEncode(fullPathName));
    }

    private static boolean supportsNewThumbnailApi() {
        ServerInfo serverInfo = SupportAccountManager.getInstance().getCurrentServerInfo();
        return serverInfo != null && supportsNewThumbnailApi(serverInfo.getVersion());
    }

    static boolean supportsNewThumbnailApi(String version) {
        return !TextUtils.isEmpty(version) && Utils.compareVersion(version, NEW_THUMBNAIL_API_VERSION) >= 0;
    }

    private static String getOldThumbnailUrl(String serverUrl, String repoId, String encodedName) {
        return serverUrl + "api2/repos/" + repoId + "/thumbnail/?p=" + encodedName + "&size=" + ThumbnailUtils.DEFAULT_THUMBNAIL_SIZE;
    }

    static String getNewThumbnailUrl(String serverUrl, String repoId, String path) {
        while (path.startsWith("/")) {
            path = path.substring(1);
        }
        return serverUrl + "thumbnail/" + repoId + "/" + ThumbnailUtils.DEFAULT_THUMBNAIL_SIZE + "/" + Uri.encode(path, "/");
    }
}
