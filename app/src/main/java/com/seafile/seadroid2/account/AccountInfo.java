package com.seafile.seadroid2.account;

import com.seafile.seadroid2.framework.util.Utils;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * This class used to manage Account information
 */
public class AccountInfo {
    public static final String SPACE_USAGE_SEPARATOR = " / ";
    private long usage;
    private long total;
    private String email;
    private String server;
    private String avatar_url;
    private String name;
    private String space_usage;// "6.0382327%"
//    private String login_id;
//    private String department;
//    private String contact_email;
//    private String institution;
//    private boolean is_staff;
//    private int file_updates_email_interval;
//    private int collaborate_email_interval;

    public long getUsage() {
        return usage;
    }

    public long getTotal() {
        return total;
    }

    public boolean isQuotaNoLimit() {
        return total < 0;
    }

    public String getEmail() {
        return email;
    }

    public String getServer() {
        return server;
    }

    public String getServerHost() {
        String s = server.substring(server.indexOf("://") + 3);
        return s.substring(0, s.indexOf('/'));
    }

    public String getName() {
        return name;
    }

    public String getDisplayName() {
        String server = Utils.stripSlashes(getServerHost());
        return Utils.assembleUserName(name, email, server);
    }

    public void setServer(String server) {
        this.server = server;
    }

    public String getSpaceUsed() {
        String strUsage = Utils.readableFileSize(usage);
        if (isQuotaNoLimit()) {
            return strUsage + SPACE_USAGE_SEPARATOR + "--";
        }

        String strTotal = Utils.readableFileSize(total);
        return strUsage + SPACE_USAGE_SEPARATOR + strTotal;
    }

    public String getAvatarUrl() {
        return avatar_url;
    }

    @Override
    public String toString() {
        return "AccountInfo{" +
                "usage=" + usage +
                ", total=" + total +
                ", email='" + email + '\'' +
                ", server='" + server + '\'' +
                ", avatar_url='" + avatar_url + '\'' +
                ", name='" + name + '\'' +
                ", space_usage='" + space_usage + '\'' +
                '}';
    }
}
