package com.seafile.seadroid2.framework.model;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Value type that represents a ServerInfo.
 */
public class ServerInfo implements Parcelable {

    private String url;
    private String version;
    private String features;
    private String encrypted_library_version;

    public ServerInfo(String url, String version, String features, String encrypted_library_version) {
        this.url = url;
        this.version = version;
        this.features = features;
        this.encrypted_library_version = encrypted_library_version;
    }

    protected ServerInfo(Parcel in) {
        url = in.readString();
        version = in.readString();
        features = in.readString();
    }

    public String getEncrypted_library_version() {
        return encrypted_library_version;
    }

    public void setEncrypted_library_version(String encrypted_library_version) {
        this.encrypted_library_version = encrypted_library_version;
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
     * @return Server version. Might be null
     */
    public String getVersion() {
        return version;
    }

    /**
     * @return Server features. Might be null
     */
    public String getFeatures() {
        return features;
    }

    public boolean isProEdition() {
        return features != null && features.contains("seafile-pro");
    }

    public boolean isEnableWiki() {
        return features != null && features.contains("wiki");
    }

    public boolean isEnableOnlyOffice() {
        return features != null && features.contains("onlyoffice");
    }

    public boolean isSearchEnabled() {
        return features != null && features.contains("file-search");
    }

    public boolean canLocalDecrypt() {
        if (TextUtils.isEmpty(version))
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
        String encrypted_library_version = obj.optString("encrypted_library_version");
        ServerInfo serverInfo = new ServerInfo(server, version, features, encrypted_library_version);
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

        ServerInfo si = (ServerInfo) obj;
        if (si.url == null || si.version == null || si.features == null)
            return false;

        return si.url.equals(this.url) && si.version.equals(this.version) && si.features.equals(this.features);
    }

    @Override
    public String toString() {
        return MoreObjects.toStringHelper(this)
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
