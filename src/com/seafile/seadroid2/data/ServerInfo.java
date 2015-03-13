package com.seafile.seadroid2.data;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Server info entity
 */
public class ServerInfo {

    private String version;

    private String features;

    private boolean isProEdition;

    public String getVersion() {
        return version;
    }

    public boolean isProEdition() {
        return isProEdition;
    }

    static ServerInfo fromJson(JSONObject obj) throws JSONException {
        ServerInfo serverInfo = new ServerInfo();
        serverInfo.version = obj.optString("version");
        serverInfo.features = obj.optString("features");
        // raw data goes like this, ["seafile-basic","seafile-pro","office-preview"]
        if (serverInfo.features.contains("seafile-pro"))
            serverInfo.isProEdition = true;

        return serverInfo;
    }

}
