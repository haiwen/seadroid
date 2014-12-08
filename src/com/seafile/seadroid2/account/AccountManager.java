package com.seafile.seadroid2.account;

import java.io.IOException;
import java.util.List;

import android.content.Context;
import android.content.SharedPreferences;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.util.Utils;
import org.json.JSONException;
import org.json.JSONObject;

public class AccountManager {
    @SuppressWarnings("unused")
    private static String DEBUG_TAG = "AccountManager";

    public static final String ACCOUNT_INFO_USAGE = "account_info_usage";
    public static final String ACCOUNT_INFO_TOTAL = "account_info_total";
    public static final String ACCOUNT_INFO_EMAIL = "account_info_email";

    public static final String SHARED_PREF_NAME = "latest_account";
    public static final String SHARED_PREF_SERVER_KEY = "com.seafile.seadroid.server";
    public static final String SHARED_PREF_EMAIL_KEY = "com.seafile.seadroid.email";
    public static final String SHARED_PREF_TOKEN_KEY = "com.seafile.seadroid.token";

    public static final String SHARED_PREF_ACCOUNT = "Account";
    public static final String SHARED_PREF_ACCOUNT_SERVER = "server";
    public static final String SHARED_PREF_ACCOUNT_EMAIL = "email";

    private SharedPreferences sharedPref; // for SHARED_PREF_NAME
    private SharedPreferences spf; // for SHARED_PREF_ACCOUNT
    private SharedPreferences.Editor editor; // sharedPref.Editor()

    private final AccountDBHelper dbHelper;
    private Context context;

    public AccountManager(Context context) {
        this.context = context;
        dbHelper = AccountDBHelper.getDatabaseHelper(context);
        sharedPref = context.getSharedPreferences(SHARED_PREF_NAME, Context.MODE_PRIVATE);
        spf = context.getSharedPreferences(SHARED_PREF_ACCOUNT, Context.MODE_PRIVATE);

        editor = sharedPref.edit();
    }

    public Account getDefaultAccount() {
        String defaultServer = spf.getString(SHARED_PREF_ACCOUNT_SERVER, "");
        String defaultEmail = spf.getString(SHARED_PREF_ACCOUNT_EMAIL, "");

        if (defaultServer.length() == 0 || defaultEmail.length() == 0)
            return null;

        return dbHelper.getAccount(defaultServer, defaultEmail);
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

    public void saveDefaultAccount(Account account) {
        // save to shared preference
        SharedPreferences.Editor editor = spf.edit();
        editor.putString(SHARED_PREF_ACCOUNT_SERVER, account.server);
        editor.putString(SHARED_PREF_ACCOUNT_EMAIL, account.email);
        editor.commit();

        // save to db
        dbHelper.saveAccount(account);
    }

    public void updateAccount(Account oldAccount, Account newAccount) {
        // save to shared preference
        SharedPreferences.Editor editor = spf.edit();
        editor.putString(SHARED_PREF_ACCOUNT_SERVER, newAccount.server);
        editor.putString(SHARED_PREF_ACCOUNT_EMAIL, newAccount.email);
        editor.commit();

        // save to db
        dbHelper.updateAccount(oldAccount, newAccount);
    }

    public void deleteAccount(Account account) {
        // TODO delete from shared preference if it is the default account ?

        // delete from db
        dbHelper.deleteAccount(account);
    }

    public  Account getDemoAccout() {
        return new Account("http://cloud.seafile.com", "demo@seafile.com", "demo", null);
    }

    public Account getLatestAccount() {
        String latest_server = sharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String latest_email = sharedPref.getString(SHARED_PREF_EMAIL_KEY, null);
        String latest_token = sharedPref.getString(SHARED_PREF_TOKEN_KEY, null);

        // When user sign out, the value of token will be null, then leads user to AccountsActivity
        if (latest_server != null && latest_token != null) {
            return new Account(latest_server, latest_email, null, latest_token);
        } else
            return null;
    }


    public void writeToSharedPreferences(Account account) {

        editor.putString(SHARED_PREF_SERVER_KEY, account.server);
        editor.putString(SHARED_PREF_EMAIL_KEY, account.email);
        editor.putString(SHARED_PREF_TOKEN_KEY, account.token);
        editor.commit();
    }

    public void clearDataFromSharedPreferences(Account account) {

        String latestServer = sharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String latestEmail = sharedPref.getString(SHARED_PREF_EMAIL_KEY, null);
        // update cache data of settings module
        String settingsServer = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        String settingsEmail = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);

        if (account.server.equals(latestServer) && account.email.equals(latestEmail)) {
            editor.putString(SHARED_PREF_SERVER_KEY, null);
            editor.putString(SHARED_PREF_EMAIL_KEY, null);
            editor.putString(SHARED_PREF_TOKEN_KEY, null);
            editor.commit();
        }
        if (account.server.equals(settingsServer) && account.email.equals(settingsEmail)) {
            SettingsManager.instance().clearCameraUploadInfo();
        }
    }

    /**
     * delete token of the current Account instance.<br>
     * If Camera Upload Service is running under the Account, stop the service.
     *
     * @param account
     */
    public void deleteTokenByAccount(Account account) {

        String latestServer = sharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String latestEmail = sharedPref.getString(SHARED_PREF_EMAIL_KEY, null);

        String signOutServer = account.getServer();
        String signOutEmail = account.getEmail();

        if (signOutServer.equals(latestServer) && signOutEmail.equals(latestEmail)) {
            // delete token
            editor.putString(SHARED_PREF_TOKEN_KEY, null);
            editor.commit();
        }

        if (signOutServer.equals(latestServer) && signOutEmail.equals(latestEmail)) {
            SettingsManager.instance().clearCameraUploadInfo();
        }

    }

    /**
     * get AccountInfo instance
     *
     * @return AccountInfo
     */
    public AccountInfo getAccountInfo() {
        long usage = spf.getLong(ACCOUNT_INFO_USAGE, 0);
        long total = spf.getLong(ACCOUNT_INFO_TOTAL, 0);
        String email = spf.getString(ACCOUNT_INFO_EMAIL, null);
        AccountInfo actInfo = new AccountInfo();
        actInfo.setUsage(usage);
        actInfo.setTotal(total);
        actInfo.setEmail(email);
        return actInfo;
    }

    /**
     * get AccountInfo from server, should check return result, it maybe null.
     * Recommend to run this method in {@link com.seafile.seadroid2.ConcurrentAsyncTask}
     * @param account
     */
    public void getAccountInfoFromServer (Account account) {
        SeafConnection seafConnection = new SeafConnection(account);
        try {
            // get Account Info from server
            String actInfo = seafConnection.getAccountInfo();
            // parse raw data
            AccountInfo accountInfo = parseAccountInfo(actInfo);
            if (accountInfo == null) return;

            // persist AccountInfo data
            SharedPreferences.Editor editor = spf.edit();
            editor.putLong(ACCOUNT_INFO_USAGE, accountInfo.getUsage());
            editor.putLong(ACCOUNT_INFO_TOTAL, accountInfo.getTotal());
            editor.putString(ACCOUNT_INFO_EMAIL, accountInfo.getEmail());
            editor.commit();

        } catch (IOException e) {
            e.printStackTrace();
        } catch (SeafException e) {
            e.printStackTrace();
        } catch (JSONException e) {
            e.printStackTrace();
        }

    }

    /**
     * parse JSON format data
     *
     * @param accountInfo
     * @return AccountInfo
     * @throws JSONException
     */
    private AccountInfo parseAccountInfo(String accountInfo) throws JSONException {
        JSONObject obj = Utils.parseJsonObject(accountInfo);
        if (obj == null)
            return null;
        return AccountInfo.fromJson(obj);
    }
}
