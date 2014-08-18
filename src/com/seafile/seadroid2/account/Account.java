package com.seafile.seadroid2.account;

import android.os.Parcel;
import android.os.Parcelable;
import android.util.Log;

import com.google.common.base.Objects;

public class Account implements Parcelable {
    private static final String DEBUG_TAG = "Account";

    // The full URL of the server, like 'http://gonggeng.org/seahub/' or 'http://gonggeng.org/'
    public String server;

    public String email;
    public String token;
    public String passwd;

    public Account() {

    }

    public Account(String server, String email) {
        this.server = server;
        this.email = email;
    }

    public Account(String server, String email, String passwd) {
        this.server = server;
        this.email = email;
        this.passwd = passwd;
    }

    public Account(String server, String email, String passwd, String token) {
        this.server = server;
        this.email = email;
        this.passwd = passwd;
        this.token = token;
    }

    public String getServerHost() {
        String s = server.substring(server.indexOf("://") + 3);
        return s.substring(0, s.indexOf('/'));
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
        if (a.server == null || a.email == null)
            return false;

        return a.server.equals(this.server) && a.email.equals(this.email);
    }

    public String getSignature() {
        return email.substring(0, 4) + " " + hashCode();
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel out, int flags) {
        out.writeString(server);
        out.writeString(email);
        out.writeString(passwd);
        out.writeString(token);
    }

    public static final Parcelable.Creator<Account> CREATOR
    = new Parcelable.Creator<Account>() {
        @Override
		public Account createFromParcel(Parcel in) {
            return new Account(in);
        }

        @Override
		public Account[] newArray(int size) {
            return new Account[size];
        }
    };

    private Account(Parcel in) {
        server = in.readString();
        email = in.readString();
        passwd = in.readString();
        token = in.readString();

        Log.d(DEBUG_TAG, String.format("%s %s %s %s", server, email, passwd, token));
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this)
            .add("server", server)
            .add("user", email)
            .toString();
    }

}
