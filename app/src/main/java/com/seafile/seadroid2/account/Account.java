package com.seafile.seadroid2.account;

import static com.seafile.seadroid2.account.AccountInfo.SPACE_USAGE_SEPARATOR;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import com.blankj.utilcode.util.EncryptUtils;
import com.google.common.base.Objects;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.util.URLs;
import com.seafile.seadroid2.framework.util.Utils;

import java.util.Locale;

public class Account extends BaseModel implements Parcelable, Comparable<Account> {
    // The full URL of the server, like 'http://gonggeng.org/seahub/' or 'http://gonggeng.org/'
    public String server;
    public String name;

    public String email;

    //single sign in
    public boolean is_shib;

    public String token;
    public String sessionKey;
    public String avatar_url;
    //timestamp
    public long login_time;

    /**
     * This field is only assigned when the user logs in, and it shouldn't actually need to be used
     */
    public long usage;
    public long total;


    public Account() {

    }

    public Account(String server, String email, String name, String avatar_url, String token, Boolean is_shib) {
        this.name = name;
        this.avatar_url = avatar_url;
        this.server = server;
        this.email = email;
        this.token = token;
        this.is_shib = is_shib;
    }


    public Account(String name, String server, String email, String avatar_url, String token, Boolean is_shib, String sessionKey, String loginTime) {
        this.server = server;
        this.name = name;
        this.email = email;
        this.avatar_url = avatar_url;
        this.token = token;
        this.sessionKey = sessionKey;
        this.is_shib = is_shib;

        if (TextUtils.isEmpty(loginTime)) {
            loginTime = "0";
        }
        this.login_time = Long.parseLong(loginTime);
    }

    public long getUsageSpace() {
        return usage;
    }

    public void setUsageSpace(long usage) {
        this.usage = usage;
    }

    public long getTotalSpace() {
        return total;
    }

    public void setTotalSpace(long total) {
        this.total = total;
    }

    public String getSpaceUsed() {
        String strUsage = Utils.readableFileSize(usage);
        if (isQuotaUnlimited()) {
            return strUsage + SPACE_USAGE_SEPARATOR + "--";
        }

        String strTotal = Utils.readableFileSize(total);
        return strUsage + SPACE_USAGE_SEPARATOR + strTotal;
    }

    /**
     * in fact, the value should be less than 0.
     * however, in some cases, it may be 0, and should also return unlimited.
     * even if the non-limit is returned, App does not need to verify "Out of quota" status.
     * and the "Out of quota" error will be returned in the file upload result.
     */
    public boolean isQuotaUnlimited() {
        return total <= 0;
    }

    public void setLoginTimestamp(long timestamp) {
        this.login_time = timestamp;
    }

    public long getLoginTimestamp() {
        return login_time;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setAvatarUrl(String avatar_url) {
        this.avatar_url = avatar_url;
    }

    public void setEmail(String email) {
        this.email = email;
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

    public String getAvatarUrl() {
        return avatar_url;
    }

    public String getName() {
        return name;
    }


    public String getServer() {
        return server;
    }

    /**
     * https://dev.xxx.com/dev/ => https://dev.xxx.com
     */
    public String getProtocolHost() {
        return URLs.getProtocolHost(server);
    }

    /**
     * https://dev.xxx.com/dev/ => dev.xxx.com/dev
     */
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
        return server.toLowerCase(Locale.getDefault()).startsWith("https");
    }

    public boolean isShib() {
        return is_shib;
    }

    public String getSessionKey() {
        return sessionKey;
    }

    public void setSessionKey(String sessionKey) {
        this.sessionKey = sessionKey;
    }


    /**
     * NOTICE: Do not modify the splicing format of this string
     */
    public String getSignature() {
        return String.format("%s (%s)", getServerNoProtocol(), email);
    }

    public String getEncryptSignature() {
        return EncryptUtils.encryptMD5ToString(getSignature());
    }


    public String getDisplayName() {
        String server = Utils.stripSlashes(getServerHost());
        return Utils.assembleUserName(name, email, server);
    }

    public android.accounts.Account getAndroidAccount() {
        return new android.accounts.Account(getSignature(), Constants.Account.ACCOUNT_TYPE);
    }

    public boolean hasValidToken() {
        return !TextUtils.isEmpty(token);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(server, email, name, token);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj == null || (obj.getClass() != this.getClass())) {
            return false;
        }

        Account newAccount = (Account) obj;
        if (newAccount.server == null || newAccount.email == null) {
            return false;
        }

        return Objects.equal(newAccount.server, this.server)
                && Objects.equal(newAccount.email, this.email)
                && Objects.equal(newAccount.name, this.name)
                && Objects.equal(newAccount.token, this.token);
    }


    @Override
    public int compareTo(Account other) {
        return this.toString().compareTo(other.toString());
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.server);
        dest.writeString(this.name);
        dest.writeString(this.email);
        dest.writeByte(this.is_shib ? (byte) 1 : (byte) 0);
        dest.writeString(this.token);
        dest.writeString(this.sessionKey);
        dest.writeString(this.avatar_url);
        dest.writeLong(this.login_time);
        dest.writeLong(this.usage);
        dest.writeLong(this.total);
    }

    public void readFromParcel(Parcel source) {
        this.server = source.readString();
        this.name = source.readString();
        this.email = source.readString();
        this.is_shib = source.readByte() != 0;
        this.token = source.readString();
        this.sessionKey = source.readString();
        this.avatar_url = source.readString();
        this.login_time = source.readLong();
        this.usage = source.readLong();
        this.total = source.readLong();
    }

    protected Account(Parcel in) {
        this.server = in.readString();
        this.name = in.readString();
        this.email = in.readString();
        this.is_shib = in.readByte() != 0;
        this.token = in.readString();
        this.sessionKey = in.readString();
        this.avatar_url = in.readString();
        this.login_time = in.readLong();
        this.usage = in.readLong();
        this.total = in.readLong();
    }

    public static final Creator<Account> CREATOR = new Creator<Account>() {
        @Override
        public Account createFromParcel(Parcel source) {
            return new Account(source);
        }

        @Override
        public Account[] newArray(int size) {
            return new Account[size];
        }
    };
}
