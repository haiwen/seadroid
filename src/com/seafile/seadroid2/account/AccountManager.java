package com.seafile.seadroid2.account;

import java.io.IOException;
import java.util.List;

import android.content.Context;
import android.content.SharedPreferences;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.util.Utils;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Account Manager.<br>
 * note the differences between {@link Account} and {@link AccountInfo}<br>
 *
 */
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

    /** used to manage multi Accounts when user switch between different Accounts */
    private SharedPreferences actMangeSharedPref;
    private SharedPreferences.Editor editor;

    /** used to persist Authorized Account info to local storage */
    private SharedPreferences authoritySharedPref;

    private final AccountDBHelper dbHelper;
    private Context context;

    public AccountManager(Context context) {
        this.context = context;
        dbHelper = AccountDBHelper.getDatabaseHelper(context);
        // used to manage multi Accounts when user switch between different Accounts
        actMangeSharedPref = context.getSharedPreferences(SHARED_PREF_NAME, Context.MODE_PRIVATE);
        editor = actMangeSharedPref.edit();

        // used to persist Authorized Account info to local storage
        authoritySharedPref = context.getSharedPreferences(SHARED_PREF_ACCOUNT, Context.MODE_PRIVATE);

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
     * get the latest Authorized Account
     *
     * @return
     */
    public Account getLatestAuthorizedAccount() {
        String defaultServer = authoritySharedPref.getString(SHARED_PREF_ACCOUNT_SERVER, "");
        String defaultEmail = authoritySharedPref.getString(SHARED_PREF_ACCOUNT_EMAIL, "");

        if (defaultServer.length() == 0 || defaultEmail.length() == 0)
            return null;

        return dbHelper.getAccount(defaultServer, defaultEmail);
    }

    /**
     * recommend to call this method in {@link com.seafile.seadroid2.ui.activity.AccountDetailActivity.LoginTask}
     *
     * @param account
     */
    public void saveAuthorizedAccount(Account account) {
        // save to shared preference
        SharedPreferences.Editor editor = authoritySharedPref.edit();
        editor.putString(SHARED_PREF_ACCOUNT_SERVER, account.server);
        editor.putString(SHARED_PREF_ACCOUNT_EMAIL, account.email);
        editor.commit();

        // save to db
        dbHelper.saveAccount(account);
    }

    /**
     * recommend to call this method when edit Account Info in {@link com.seafile.seadroid2.ui.activity.AccountDetailActivity.LoginTask}
     *
     * @param oldAccount
     * @param newAccount
     */
    public void updateAuthorizedAccount(Account oldAccount, Account newAccount) {
        // save to shared preference
        SharedPreferences.Editor editor = authoritySharedPref.edit();
        editor.putString(SHARED_PREF_ACCOUNT_SERVER, newAccount.server);
        editor.putString(SHARED_PREF_ACCOUNT_EMAIL, newAccount.email);
        editor.commit();

        // save to db
        dbHelper.updateAccount(oldAccount, newAccount);
    }

    public  Account getDemoAccount() {
        return new Account("http://cloud.seafile.com", "demo@seafile.com", "demo", null);
    }

    public Account getCurrentAccount() {
        String latest_server = actMangeSharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String latest_email = actMangeSharedPref.getString(SHARED_PREF_EMAIL_KEY, null);
        String latest_token = actMangeSharedPref.getString(SHARED_PREF_TOKEN_KEY, null);

        // When user sign out, the value of token will be null, then leads user to AccountsActivity
        if (latest_server != null && latest_token != null) {
            return new Account(latest_server, latest_email, null, latest_token);
        } else
            return null;
    }

    /**
     * update current Account info from SharedPreference<br>
     * <strong>current</strong> means the Account is now in using at the foreground
     *
     * @param account
     */
    public void updateCurrentAccount(Account account) {

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

        String latestServer = actMangeSharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String latestEmail = actMangeSharedPref.getString(SHARED_PREF_EMAIL_KEY, null);
        // update cache data of settings module
        String settingsServer = actMangeSharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        String settingsEmail = actMangeSharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);

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

    public void deleteAccountFromDB(Account account) {
        // TODO delete from shared preference if it is the default account ?

        // delete from db
        dbHelper.deleteAccount(account);
    }

    /**
     * when user sign out, delete authorized information of the current Account instance.<br>
     * If Camera Upload Service is running under the Account, stop the service.
     *
     */
    public void signOutCurrentAccount() {

        // TODO delete data in Shared_prefs

        // TODO delete data in database

        // TODO delete data in AccountsActivity

        /*String currentServer = actMangeSharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String currentEmail = actMangeSharedPref.getString(SHARED_PREF_EMAIL_KEY, null);

        String signOutServer = authoritySharedPref.getString(SHARED_PREF_ACCOUNT_SERVER, null);
        String signOutEmail = authoritySharedPref.getString(SHARED_PREF_ACCOUNT_EMAIL, null);

        if (signOutServer.equals(currentServer) && signOutEmail.equals(currentEmail)) {
            // set current account to null in actMangeSharedPref
            editor.putString(SHARED_PREF_SERVER_KEY, null);
            editor.putString(SHARED_PREF_EMAIL_KEY, null);
            editor.putString(SHARED_PREF_TOKEN_KEY, null);
            editor.commit();

            // clear camera upload info
            SettingsManager.instance().clearCameraUploadInfo();

        }*/

    }

    /**
     * get AccountInfo instance from local SharedPreference
     *
     * @return AccountInfo
     */
    public AccountInfo getAccountInfoFromSharedPreference() {
        long usage = authoritySharedPref.getLong(ACCOUNT_INFO_USAGE, 0);
        long total = authoritySharedPref.getLong(ACCOUNT_INFO_TOTAL, 0);
        String email = authoritySharedPref.getString(ACCOUNT_INFO_EMAIL, null);
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
    public void getAccountInfoFromServer(Account account) {
        SeafConnection seafConnection = new SeafConnection(account);
        try {
            // get Account Info from server
            String actInfo = seafConnection.getAccountInfo();
            // parse raw data
            AccountInfo accountInfo = parseAccountInfo(actInfo);
            if (accountInfo == null) return;

            // persist AccountInfo data
            SharedPreferences.Editor editor = authoritySharedPref.edit();
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
