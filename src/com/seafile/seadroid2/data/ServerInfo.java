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

    public ServerInfo(String url, String version, String features) {
        this.url = url;
        this.version = version;
        this.features = features;
    }

    public String getVersion() {
        return version;
    }

    public String getFeatures() {
        return features;
    }

    public boolean isProEdition() {
        return isProEdition;
    }

    public void setProEdition(boolean isProEdition) {
        this.isProEdition = isProEdition;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public static ServerInfo fromJson(JSONObject obj) throws JSONException {
        String version = obj.optString("version");
        String features = obj.optString("features");
        String url = "not applicable";
        ServerInfo serverInfo = new ServerInfo(url, version, features);
        /** raw data goes like this, ["seafile-basic","seafile-pro","office-preview"] */
        serverInfo.setProEdition(features.contains("seafile-pro"));

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
