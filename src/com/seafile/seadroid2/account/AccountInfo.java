package com.seafile.seadroid2.account;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * This class used to manage Account information
 *
 * Created by Logan on 14/12/8.
 */
public class AccountInfo {
    private static final String DEBUG_TAG = "AccountInfo";

    private long usage;
    private long total;
    private String email;

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
}
