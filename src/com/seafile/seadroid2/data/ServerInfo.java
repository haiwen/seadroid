package com.seafile.seadroid2.data;

import android.os.Parcel;
import android.os.Parcelable;
import com.google.common.base.Objects;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Value type that represents a ServerInfo.
 */
public class ServerInfo implements Parcelable{

    private String url;
    private String version;
    private String features;
    private boolean proEdition;
    private boolean searchEnabled;

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

    public boolean proEdition() {
        return proEdition;
    }

    public void setProEdition(boolean proEdition) {
        this.proEdition = proEdition;
    }

    public boolean searchEnabled() {
        return searchEnabled;
    }

    public void setSearchEnabled(boolean searchEnabled) {
        this.searchEnabled = searchEnabled;
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
        /* actually there is no url node in responded json data
         * but it is useful to use url to represent the server,
         * so url should be explicitly assigned a valid value by calling {@link #setUrl}
         * instead of using the hard coded value.
         * If url assignment was not handled by a future maintainer, an error may occur */
        String url = "not applicable";
        ServerInfo serverInfo = new ServerInfo(url, version, features);
        // raw data goes like "features":["seafile-basic","seafile-pro","office-preview","file-search"]
        serverInfo.setProEdition(features.contains("seafile-pro"));
        serverInfo.setSearchEnabled(features.contains("file-search"));

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

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(url);
        dest.writeString(version);
        dest.writeString(features);
    }
}
