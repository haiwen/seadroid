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
    private String server;

    private AccountInfo() {}

    public static  AccountInfo fromJson(JSONObject accountInfo, String server) throws JSONException {
        AccountInfo info = new AccountInfo();
        info.server = server;
        info.usage = accountInfo.getLong("usage");
        info.total = accountInfo.getLong("total");
        info.email = accountInfo.getString("email");

        return info;
    }

    public long getUsage() {
        return usage;
    }

    public long getTotal() {
        return total;
    }

    public String getEmail() {
        return email;
    }

    public String getServer() {
        return server;
    }

    public String getSpaceUsed() {
        String strUsage = Utils.readableFileSize(usage);
        String strTotal = Utils.readableFileSize(total);
        return strUsage + SPACE_USAGE_SEPERATOR + strTotal;
    }

}
