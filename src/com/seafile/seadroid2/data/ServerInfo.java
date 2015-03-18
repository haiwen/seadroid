package com.seafile.seadroid2.data;

import com.google.common.base.Objects;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Server info entity
 */
public class ServerInfo {

    private String url;
    private String version;
    private String features;
    private boolean isProEdition;

    public String getVersion() {
        return version;
    }

    public boolean isProEdition() {
        return isProEdition;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    static ServerInfo fromJson(JSONObject obj) throws JSONException {
        ServerInfo serverInfo = new ServerInfo();
        serverInfo.version = obj.optString("version");
        serverInfo.features = obj.optString("features");
        /** raw data goes like this, ["seafile-basic","seafile-pro","office-preview"] */
        if (serverInfo.features.contains("seafile-pro"))
            serverInfo.isProEdition = true;

        serverInfo.url = "not applicable";
        return serverInfo;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(url, version, features);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;

        ServerInfo si = (ServerInfo)obj;
        if (si.url == null || si.version == null || si.features == null)
            return false;

        return si.url.equals(this.url) && si.version.equals(this.version) && si.features.equals(this.features);
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this)
                .add("url", url)
                .add("version", version)
                .add("features", features)
                .toString();
    }

}
