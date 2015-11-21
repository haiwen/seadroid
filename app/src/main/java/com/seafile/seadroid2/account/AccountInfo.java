package com.seafile.seadroid2.account;

import com.seafile.seadroid2.util.Utils;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * This class used to manage Account information
 *
 */
public class AccountInfo {
    private static final String DEBUG_TAG = "AccountInfo";

    public static final String SPACE_USAGE_SEPERATOR = " / ";
    private long usage;
    private long total;
    private String email;

    // server doesn`t exist in API JOSN data,
    // it was used together with email to identify the uniqueness
    //  of the AccountInfo instance
    // to assign value to it, explicitly call its setter method.
    private String server;

    public AccountInfo() {}

    public static  AccountInfo fromJson(JSONObject accountInfo) throws JSONException {
        AccountInfo info = new AccountInfo();
        info.usage = accountInfo.getLong("usage");
        info.total = accountInfo.getLong("total");
        info.email = accountInfo.getString("email");

        return info;
    }

    public long getUsage() {
        return usage;
    }

    public void setUsage(long usage) {
        this.usage = usage;
    }

    public long getTotal() {
        return total;
    }

    public void setTotal(long total) {
        this.total = total;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getServer() {
        return server;
    }

    public void setServer(String server) {
        this.server = server;
    }

    public String getSpaceUsed() {
        String strUsage = Utils.readableFileSize(usage);
        String strTotal = Utils.readableFileSize(total);
        return strUsage + SPACE_USAGE_SEPERATOR + strTotal;
    }

}
