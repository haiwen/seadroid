package com.seafile.seadroid2.account;

import android.accounts.AccountManagerCallback;
import android.accounts.AccountManagerFuture;
import android.os.Bundle;
import android.os.Handler;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.io.http.IO;
import com.seafile.seadroid2.util.sp.SPs;

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
        List<Account> list = new ArrayList<Account>();
        String currentAccountName = getCurrentAccountName();

        android.accounts.Account[] availableAccounts = accountManager.getAccountsByType(Constants.Account.ACCOUNT_TYPE);
        for (android.accounts.Account availableAccount : availableAccounts) {
            Account a = getSeafileAccount(availableAccount);
            if (!TextUtils.isEmpty(currentAccountName) && a.getSignature().equals(currentAccountName)) {
                a.is_selected = true;
            }

            list.add(a);
        }

        return list;
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
    public void saveCurrentAccount(String accountName) {
        SPs.put(Constants.SP.ACCOUNT_CURRENT, accountName);

        //IMPORTANT
        //reset IO Singleton
        IO.resetSingleton();
    }

    @Nullable
    public Account getCurrentAccount() {
        String name = SPs.getString(Constants.SP.ACCOUNT_CURRENT);

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
        return SPs.getString(Constants.SP.ACCOUNT_CURRENT);
    }

    @NonNull
    public Account getSeafileAccount(android.accounts.Account androidAccount) {
        String server = accountManager.getUserData(androidAccount, Authenticator.KEY_SERVER_URI);
        String email = accountManager.getUserData(androidAccount, Authenticator.KEY_EMAIL);
        String name = accountManager.getUserData(androidAccount, Authenticator.KEY_NAME);
        String avatarUrl = accountManager.getUserData(androidAccount, Authenticator.KEY_AVATAR_URL);
        boolean is_shib = accountManager.getUserData(androidAccount, Authenticator.KEY_SHIB) != null;
        String token = accountManager.peekAuthToken(androidAccount, Authenticator.AUTHTOKEN_TYPE);
        String session_key = accountManager.getUserData(androidAccount, Authenticator.SESSION_KEY);
        return new Account(name, server, email, avatarUrl, token, is_shib, session_key);
    }

    public void setServerInfo(Account account, ServerInfo serverInfo) {
        accountManager.setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_URI, serverInfo.getUrl());
        accountManager.setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_VERSION, serverInfo.getVersion());
        accountManager.setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_FEATURES, serverInfo.getFeatures());
    }

    public void setUserData(final android.accounts.Account account, final String key, final String value) {
        accountManager.setUserData(account, key, value);
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
        String server = accountManager.getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_URI);
        String version = accountManager.getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_VERSION);
        String features = accountManager.getUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_FEATURES);
        return new ServerInfo(server, version, features);
    }


    /**
     * when user sign out, delete authorized information of the current Account instance.<br>
     * If Camera Upload Service is running under the Account, stop the service.
     */
    public void signOutAccount(Account account) {
        if (account == null || TextUtils.isEmpty(account.getToken())) {
            return;
        }

        saveCurrentAccount(null);

        accountManager.invalidateAuthToken(Constants.Account.ACCOUNT_TYPE, account.getToken());

        CameraUploadManager cameraManager = new CameraUploadManager();
        // disable camera upload if on this account
        Account camAccount = cameraManager.getCameraAccount();
        if (camAccount != null && camAccount.equals(account)) {
            cameraManager.disableCameraUpload();
        }
    }

}
