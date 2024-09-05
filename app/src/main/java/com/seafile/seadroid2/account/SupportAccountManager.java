package com.seafile.seadroid2.account;

import android.accounts.AccountManagerCallback;
import android.accounts.AccountManagerFuture;
import android.os.Bundle;
import android.os.Handler;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;

import java.util.ArrayList;
import java.util.List;

public class SupportAccountManager {

    private static volatile SupportAccountManager singleton = null;

    public static SupportAccountManager getInstance() {
        if (singleton == null) {
            synchronized (SupportAccountManager.class) {
                if (singleton == null) {
                    singleton = new SupportAccountManager();
                }
            }
        }
        return singleton;
    }

    private final android.accounts.AccountManager accountManager;

    private SupportAccountManager() {
        accountManager = android.accounts.AccountManager.get(SeadroidApplication.getAppContext());
    }

    @NonNull
    public List<Account> getAccountList() {
        List<Account> list = new ArrayList<>();
        String currentAccountName = getCurrentAccountName();

        android.accounts.Account[] availableAccounts = accountManager.getAccountsByType(Constants.Account.ACCOUNT_TYPE);
        for (android.accounts.Account availableAccount : availableAccounts) {
            Account a = getSeafileAccount(availableAccount);
            if (!TextUtils.isEmpty(currentAccountName) && a.getSignature().equals(currentAccountName)) {
                a.is_checked = true;
            }

            list.add(a);
        }

        return list;
    }

    @Nullable
    public Account getSpecialAccount(String accountSignature) {

        android.accounts.Account[] availableAccounts = accountManager.getAccountsByType(Constants.Account.ACCOUNT_TYPE);
        for (android.accounts.Account availableAccount : availableAccounts) {
            Account a = getSeafileAccount(availableAccount);
            if (!TextUtils.isEmpty(accountSignature) && a.getSignature().equals(accountSignature)) {
                return a;
            }
        }

        return null;
    }

    @NonNull
    public List<Account> getSignedInAccountList() {
        List<Account> list = new ArrayList<>();
        android.accounts.Account[] availableAccounts = accountManager.getAccountsByType(Constants.Account.ACCOUNT_TYPE);
        for (android.accounts.Account availableAccount : availableAccounts) {
            Account a = getSeafileAccount(availableAccount);
            if (a.hasValidToken())
                list.add(a);
        }
        return list;
    }

    /**
     * save current Account info to SharedPreference<br>
     * <strong>current</strong> means the Account is now in using at the foreground if has multiple accounts
     */
    public void saveCurrentAccount(String accountSignature) {

        Account sAccount = getSpecialAccount(accountSignature);
        if (sAccount == null) {
            return;
        }

        //
        DataStoreManager.getCommonSharePreference().writeString(DataStoreKeys.KEY_CURRENT_ACCOUNT, accountSignature);

        //
        setAuthToken(sAccount.getAndroidAccount(), Constants.Account.ACCOUNT_TYPE, sAccount.getToken());
    }

    @Nullable
    public Account getCurrentAccount() {
        String name = DataStoreManager.getCommonSharePreference().readString(DataStoreKeys.KEY_CURRENT_ACCOUNT);

        if (!TextUtils.isEmpty(name)) {
            List<Account> list = getAccountList();
            for (Account a : list) {
                if (a.hasValidToken() && a.getSignature().equals(name)) {
                    return a;
                }
            }
        }

        return null;
    }

    public String getCurrentAccountName() {
        return DataStoreManager.getCommonSharePreference().readString(DataStoreKeys.KEY_CURRENT_ACCOUNT);
    }

    @NonNull
    public Account getSeafileAccount(android.accounts.Account androidAccount) {
        String server = accountManager.getUserData(androidAccount, Authenticator.KEY_SERVER_URI);
        String email = accountManager.getUserData(androidAccount, Authenticator.KEY_EMAIL);
        String name = accountManager.getUserData(androidAccount, Authenticator.KEY_NAME);
        String avatarUrl = accountManager.getUserData(androidAccount, Authenticator.KEY_AVATAR_URL);
        boolean isShib = accountManager.getUserData(androidAccount, Authenticator.KEY_SHIB) != null;
        String token = accountManager.peekAuthToken(androidAccount, Authenticator.AUTHTOKEN_TYPE);
        String sessionKey = accountManager.getUserData(androidAccount, Authenticator.SESSION_KEY);
        String loginTime = accountManager.getUserData(androidAccount, Authenticator.LOGIN_TIME);
        String totalSpace = accountManager.getUserData(androidAccount, Authenticator.SPACE_TOTAL);
        String usageSpace = accountManager.getUserData(androidAccount, Authenticator.SPACE_USAGE);

        Account account = new Account();
        account.name = name;
        account.server = server;
        account.email = email;
        account.avatar_url = avatarUrl;
        account.token = token;
        account.is_shib = isShib;
        account.sessionKey = sessionKey;

        if (TextUtils.isEmpty(totalSpace)) {
            account.setTotalSpace(0L);
        } else {
            account.setTotalSpace(Long.parseLong(totalSpace));
        }

        if (TextUtils.isEmpty(totalSpace)) {
            account.setUsageSpace(0L);
        } else {
            account.setUsageSpace(Long.parseLong(usageSpace));
        }

        if (TextUtils.isEmpty(totalSpace)) {
            account.setLoginTimestamp(0L);
        } else {
            account.setLoginTimestamp(Long.parseLong(loginTime));
        }


        return account;
    }


    public boolean addAccountExplicitly(android.accounts.Account account, String password, Bundle userdata) {
        return accountManager.addAccountExplicitly(account, password, userdata);
    }

    public AccountManagerFuture<Boolean> removeAccount(final android.accounts.Account account,
                                                       AccountManagerCallback<Boolean> callback, Handler handler) {
        return accountManager.removeAccount(account, callback, handler);
    }

    public void setAuthToken(android.accounts.Account account, final String authTokenType, final String authToken) {
        accountManager.setAuthToken(account, authTokenType, authToken);
    }


    public void setServerInfo(Account account, ServerInfo serverInfo) {
        setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_URI, serverInfo.getUrl());
        setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_VERSION, serverInfo.getVersion());
        setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_ENCRYPTED_VERSION, serverInfo.getEncrypted_library_version());
        setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_FEATURES, serverInfo.getFeatures());
    }

    public void setUserData(android.accounts.Account account, String key, String value) {
        accountManager.setUserData(account, key, value);
    }

    public String getUserData(final android.accounts.Account account, final String key) {
        return accountManager.getUserData(account, key);
    }

    /**
     * Return cached ServerInfo
     *
     * @return ServerInfo. Will never be null.
     */
    @NonNull
    public ServerInfo getServerInfo(Account account) {
        String server = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_URI);
        String version = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_VERSION);
        String features = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_FEATURES);
        String encrypted_library_version = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_ENCRYPTED_VERSION);
        return new ServerInfo(server, version, features, encrypted_library_version);
    }

    @Nullable
    public ServerInfo getCurrentServerInfo() {
        Account account = getCurrentAccount();
        if (account == null) {
            return null;
        }

        String server = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_URI);
        String version = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_VERSION);
        String features = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_FEATURES);
        String encrypted_library_version = getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_ENCRYPTED_VERSION);
        return new ServerInfo(server, version, features, encrypted_library_version);
    }


    /**
     * when user sign out, delete authorized information of the current Account instance.<br>
     * If Camera Upload Service is running under the Account, stop the service.
     */
    public void signOutAccount(Account account) {
        if (account == null || TextUtils.isEmpty(account.getToken())) {
            return;
        }


        //invalidate auth token
        accountManager.invalidateAuthToken(Constants.Account.ACCOUNT_TYPE, account.getToken());

    }

}
