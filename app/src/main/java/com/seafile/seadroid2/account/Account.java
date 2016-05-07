package com.seafile.seadroid2.account;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;
import android.util.Log;

import com.google.common.base.Objects;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.util.Utils;

public class Account implements Parcelable, Comparable<Account> {
    private static final String DEBUG_TAG = "Account";

    /**
     * Type of the account (currently there is only one type)
     */
    public final static String ACCOUNT_TYPE = BuildConfig.ACCOUNT_TYPE;

    // The full URL of the server, like 'http://gonggeng.org/seahub/' or 'http://gonggeng.org/'
    public final String server;

    public final String email;
    public String token;

    public Account(String server, String email, String token) {
        this.server = server;
        this.email = email;
        this.token = token;
    }

    public String getServerHost() {
        String s = server.substring(server.indexOf("://") + 3);
        return s.substring(0, s.indexOf('/'));
    }

    public String getServerDomainName() {
        String dn = getServerHost();
        // strip port, like :8000 in 192.168.1.116:8000
        if (dn.contains(":"))
            dn = dn.substring(0, dn.indexOf(':'));
        return dn;
    }

    public String getEmail() {
        return email;
    }

    public String getServer() {
        return server;
    }

    public String getServerNoProtocol() {
        String result = server.substring(server.indexOf("://") + 3);
        if (result.endsWith("/"))
            result = result.substring(0, result.length() - 1);
        return result;
    }

    public String getToken() {
        return token;
    }

    public boolean isHttps() {
        return server.startsWith("https");
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(server, email);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;

        Account a = (Account)obj;
        if (a.server == null || a.email == null || a.token == null)
            return false;

        return a.server.equals(this.server) && a.email.equals(this.email);
    }

    public String getSignature() {
        return String.format("%s (%s)", getServerNoProtocol(), email);
    }

    public String getDisplayName() {
        String server = Utils.stripSlashes(getServerHost());
        return Utils.assembleUserName(email, server);
    }

    public android.accounts.Account getAndroidAccount() {
        return new android.accounts.Account(getSignature(), ACCOUNT_TYPE);
    }

    public boolean hasValidToken() {
        return !TextUtils.isEmpty(token);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel out, int flags) {
        out.writeString(server);
        out.writeString(email);
        out.writeString(token);
    }

    public static final Parcelable.Creator<Account> CREATOR
    = new Parcelable.Creator<Account>() {
        public Account createFromParcel(Parcel in) {
            return new Account(in);
        }

        public Account[] newArray(int size) {
            return new Account[size];
        }
    };

    private Account(Parcel in) {
        server = in.readString();
        email = in.readString();
        token = in.readString();

        Log.d(DEBUG_TAG, String.format("%s %s %s", server, email, token));
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this)
            .add("server", server)
            .add("user", email)
            .toString();
    }

    @Override
    public int compareTo(Account other) {
        return this.toString().compareTo(other.toString());
    }
}
