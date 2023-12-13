package com.seafile.seadroid2.account;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.cameraupload.CameraUploadManager;
import com.seafile.seadroid2.data.ServerInfo;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

/**
 * 2023-3-28 Convert to a proxy class
 * <p>
 * Account Manager.<br>
 * note the differences between {@link Account} and {@link AccountInfo}<br>
 */
public class AccountManager {
    WeakReference<Context> contextWeakReference = null;

    public AccountManager(Context context) {
        contextWeakReference = new WeakReference<>(context);
    }

    public List<Account> getAccountList() {
        return SupportAccountManager.getInstance().getAccountList();
    }

    public List<Account> getSignedInAccountList() {
        return SupportAccountManager.getInstance().getSignedInAccountList();
    }

    @Nullable
    public Account getCurrentAccount() {
        return SupportAccountManager.getInstance().getCurrentAccount();
    }

    public Account getSeafileAccount(android.accounts.Account androidAccount) {

        return SupportAccountManager.getInstance().getSeafileAccount(androidAccount);
    }

    public void setServerInfo(Account account, ServerInfo serverInfo) {
        SupportAccountManager.getInstance().setServerInfo(account, serverInfo);
    }

    /**
     * Return cached ServerInfo
     *
     * @param account
     * @return ServerInfo. Will never be null.
     */
    public ServerInfo getServerInfo(Account account) {
        return SupportAccountManager.getInstance().getServerInfo(account);
    }

    /**
     * save current Account info to SharedPreference<br>
     * <strong>current</strong> means the Account is now in using at the foreground if has multiple accounts
     *
     * @param accountName
     */
    public void saveCurrentAccount(String accountName) {
        SupportAccountManager.getInstance().saveCurrentAccount(accountName);
    }

    /**
     * when user sign out, delete authorized information of the current Account instance.<br>
     * If Camera Upload Service is running under the Account, stop the service.
     */
    public void signOutAccount(Account account) {
        SupportAccountManager.getInstance().signOutAccount(account);
    }

    /**
     * get all email texts from database in order to auto complete email address
     *
     * @return
     */
    public ArrayList<String> getAccountAutoCompleteTexts() {
        return SupportAccountManager.getInstance().getAccountAutoCompleteTexts();
    }
}