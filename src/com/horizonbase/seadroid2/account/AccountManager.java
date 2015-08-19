package com.horizonbase.seadroid2.account;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;

import org.json.JSONException;
import org.json.JSONObject;


/**
 * Account Manager.<br>
 * note the differences between {@link Account} and {@link AccountInfo}<br>
 *
 */
import com.google.common.collect.Lists;
import com.horizonbase.seadroid2.SettingsManager;
import com.horizonbase.seadroid2.cameraupload.CameraUploadService;
import com.horizonbase.seadroid2.util.Utils;

public class AccountManager {
    @SuppressWarnings("unused")
    private static String DEBUG_TAG = "AccountManager";

    public static final String SHARED_PREF_NAME = "latest_account";
    public static final String SHARED_PREF_SERVER_KEY = "com.horizonbase.seadroid.server";
    public static final String SHARED_PREF_EMAIL_KEY = "com.horizonbase.seadroid.email";
    public static final String SHARED_PREF_TOKEN_KEY = "com.horizonbase.seadroid.token";

    public static final String INVALID_TOKEN = "not_applicable";

    /** used to manage multi Accounts when user switch between different Accounts */
    private SharedPreferences actMangeSharedPref;
    private SharedPreferences.Editor editor;

    private final AccountDBHelper dbHelper;
    private Context ctx;

    public AccountManager(Context context) {
        this.ctx = context;
        dbHelper = AccountDBHelper.getDatabaseHelper(context);
        // used to manage multi Accounts when user switch between different Accounts
        actMangeSharedPref = ctx.getSharedPreferences(SHARED_PREF_NAME, Context.MODE_PRIVATE);
        editor = actMangeSharedPref.edit();

    }

    public Account getAccountBySignature(String signature) {
        List<Account> accounts = dbHelper.getAccountList();
        for (int i = 0; i < accounts.size(); ++i) {
            if (signature.equals(accounts.get(i).getSignature())) {
                return accounts.get(i);
            }
        }
        return null;
    }

    public List<Account> getAccountList() {
        return dbHelper.getAccountList();
    }

    /**
     * save sign in account to database
     *
     * @param account
     */
    public void saveAccountToDB(Account account) {
        // save to db
        dbHelper.saveAccount(account);
    }

    /**
     * update account info from database
     *
     * @param oldAccount
     * @param newAccount
     */
    public void updateAccountFromDB(Account oldAccount, Account newAccount) {

        dbHelper.updateAccount(oldAccount, newAccount);
    }

    public Account getCurrentAccount() {
        String currentServer = actMangeSharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String currentEmail = actMangeSharedPref.getString(SHARED_PREF_EMAIL_KEY, null);
        String currentToken = actMangeSharedPref.getString(SHARED_PREF_TOKEN_KEY, null);

        // When user sign out, the value of token will be null, then leads user to AccountsActivity
        if (currentServer != null && currentToken != null) {
            return new Account(currentServer, currentEmail, null, currentToken);
        } else
            return null;
    }

    /**
     * save current Account info to SharedPreference<br>
     * <strong>current</strong> means the Account is now in using at the foreground if has multiple accounts
     *
     * @param account
     */
    public void saveCurrentAccount(Account account) {

        editor.putString(SHARED_PREF_SERVER_KEY, account.server);
        editor.putString(SHARED_PREF_EMAIL_KEY, account.email);
        editor.putString(SHARED_PREF_TOKEN_KEY, account.token);
        editor.commit();
    }

    /**
     * delete a selected Account info from SharedPreference
     *
     * @param account
     */
    public void deleteAccountFromSharedPreference(Account account) {

        String currentServer = actMangeSharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String currentEmail = actMangeSharedPref.getString(SHARED_PREF_EMAIL_KEY, null);

        if (account.server.equals(currentServer) && account.email.equals(currentEmail)) {
            editor.putString(SHARED_PREF_SERVER_KEY, null);
            editor.putString(SHARED_PREF_EMAIL_KEY, null);
            editor.putString(SHARED_PREF_TOKEN_KEY, null);
            editor.commit();
        }

    }

    public void deleteCameraUploadSettingsByAccount(Account account) {
        // update cache data of settings module
        String settingsServer = actMangeSharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        String settingsEmail = actMangeSharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);

        if (account.server.equals(settingsServer) && account.email.equals(settingsEmail)) {
            SettingsManager.instance().clearCameraUploadInfo();
        }
    }

    public void deleteAccountFromDB(Account account) {

        // delete from db
        dbHelper.deleteAccount(account);
    }

    /**
     * when user sign out, delete authorized information of the current Account instance.<br>
     * If Camera Upload Service is running under the Account, stop the service.
     *
     */
    public void signOutCurrentAccount() {

        Account currentAccount =  getCurrentAccount();

        // delete token of the account from database
        Account accountWithoutToken = new Account(currentAccount.getServer(), currentAccount.getEmail(), null, INVALID_TOKEN);
        updateAccountFromDB(currentAccount, accountWithoutToken);

        // delete data in Shared_prefs
        deleteAccountFromSharedPreference(currentAccount);

        // stop camera upload service if on
        stopCamerUploadServiceByAccount(currentAccount);

        // keep Gesture lock settings

    }

    /**
     * turn off camera upload service of the deleted account if it was turned on before
     *
     * @param account
     */
    public void stopCamerUploadServiceByAccount(Account account) {
        String camerUploadEmail = SettingsManager.instance().getCameraUploadAccountEmail();
        String cameraUploadServer = SettingsManager.instance().getCameraUploadAccountServer();

        if (camerUploadEmail == null) {
            return;
        }

        // stop camera upload service
        if (camerUploadEmail.equals(account.getEmail()) && cameraUploadServer.equals(account.getServer())) {
            Intent cameraUploadIntent = new Intent(ctx, CameraUploadService.class);
            ctx.stopService(cameraUploadIntent);
        }
    }

    /**
     * parse JSON format data
     *
     * @param accountInfo
     * @return AccountInfo
     * @throws JSONException
     */
    public AccountInfo parseAccountInfo(String accountInfo) throws JSONException {
        JSONObject obj = Utils.parseJsonObject(accountInfo);
        if (obj == null)
            return null;
        return AccountInfo.fromJson(obj);
    }

    /**
    * get all email texts from database in order to auto complete email address
    *
    * @return
    */
    public ArrayList<String> getAccountAutoCompleteTexts() {
        ArrayList<String> autoCompleteTexts = Lists.newArrayList();

        List<Account> accounts = dbHelper.getAccountList();

        if (accounts == null) return null;
        for (Account act : accounts) {
            if (!autoCompleteTexts.contains(act.getEmail()))
                autoCompleteTexts.add(act.getEmail());
        }
        return autoCompleteTexts;
    }
}
