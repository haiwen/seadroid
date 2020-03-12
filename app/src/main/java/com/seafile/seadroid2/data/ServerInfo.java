package com.seafile.seadroid2.data;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

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

    public ServerInfo(String url, String version, String features) {
        this.url = url;
        this.version = version;
        this.features = features;
    }

    protected ServerInfo(Parcel in) {
        url = in.readString();
        version = in.readString();
        features = in.readString();
    }

    public static final Creator<ServerInfo> CREATOR = new Creator<ServerInfo>() {
        @Override
        public ServerInfo createFromParcel(Parcel in) {
            return new ServerInfo(in);
        }

        @Override
        public ServerInfo[] newArray(int size) {
            return new ServerInfo[size];
        }
    };

    /**
     *
     * @return Server version. Might be null
     */
    public String getVersion() {
        return version;
    }

    /**
     *
     * @return Server features. Might be null
     */
    public String getFeatures() {
        return features;
    }

    public boolean isProEdition() {
        return features != null && features.contains("seafile-pro");
    }

    public boolean isSearchEnabled() {
        return features != null && features.contains("file-search");
    }

    public boolean canLocalDecrypt() {
        if (TextUtils.isEmpty(version)
                || version.length() != 5)
            return false;

        final String realVersion = version.replaceAll("[.]", "");
        final int versionCode = Integer.parseInt(realVersion);

        return versionCode >= 510;
    }

    public String getUrl() {
        return url;
    }

    public static ServerInfo fromJson(JSONObject obj, String server) throws JSONException {
        String version = obj.optString("version");
        String features = obj.optString("features");
        ServerInfo serverInfo = new ServerInfo(server, version, features);
        // raw data goes like "features":["seafile-basic","seafile-pro","office-preview","file-search"]
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
