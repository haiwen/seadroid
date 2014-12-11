package com.seafile.seadroid2.account;

import java.io.IOException;
import java.util.List;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Handler;
import android.os.Message;
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

    public static final String SHARED_PREF_NAME = "latest_account";
    public static final String SHARED_PREF_SERVER_KEY = "com.seafile.seadroid.server";
    public static final String SHARED_PREF_EMAIL_KEY = "com.seafile.seadroid.email";
    public static final String SHARED_PREF_TOKEN_KEY = "com.seafile.seadroid.token";

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

        Account currentAccount =  getCurrentAccount();
        AccountInfo currentAccountInfo = getCurrentAccountInfo();
        // delete data in Shared_prefs
        deleteAccountFromSharedPreference(currentAccount);

        // delete camera upload settings of this account if has
        deleteCameraUploadSettingsByAccount(currentAccount);

        // TODO turn off Gesture lock settings?

        // delete account data in database
        deleteAccountFromDB(currentAccount);

        // delete account-info data in database
        deleteAccountInfo(currentAccountInfo);

    }

    public static final int REQUEST_ACCOUNT_INFO_FAILED = 0;
    public static final int REQUEST_ACCOUNT_INFO_SUCCESSFUL = 1;

    /**
     * get AccountInfo from server, should check return result, it maybe null.
     * Recommend to run this method in {@link com.seafile.seadroid2.ConcurrentAsyncTask}
     * @param account
     */
    private void doRequestAccountInfo(Account account, Handler handler) {
        SeafConnection seafConnection = new SeafConnection(account);
        try {
            // get account info from server
            String actInfo = seafConnection.getAccountInfo();
            // parse raw data
            AccountInfo accountInfo = parseAccountInfo(actInfo);
            if (accountInfo == null) {
                handler.sendEmptyMessage(REQUEST_ACCOUNT_INFO_FAILED);
                return;
            }

            accountInfo.setServer(account.getServer());

            // save to database
            saveAccountInfo(accountInfo);

            Message msg = new Message();
            msg.what = REQUEST_ACCOUNT_INFO_SUCCESSFUL;
            msg.obj = accountInfo;
            handler.sendMessage(msg);

        } catch (IOException e) {
            e.printStackTrace();
        } catch (SeafException e) {
            e.printStackTrace();
        } catch (JSONException e) {
            e.printStackTrace();
        }

    }

    /**
     * get current account info from database
     *
     * @return
     */
    public AccountInfo getCurrentAccountInfo() {
        String server = getCurrentAccount().getServer();
        String email = getCurrentAccount().getEmail();
        return dbHelper.getAccountInfo(server, email);

    }

    /**
     * save account info to database
     *
     * @param accountInfo
     */
    private void saveAccountInfo(AccountInfo accountInfo) {
        if (accountInfo == null) return;
        dbHelper.saveAccountInfo(accountInfo);
    }

    /**
     * delete account info from database
     *
     * @param accountInfo
     */
    private void deleteAccountInfo(AccountInfo accountInfo) {
        if (accountInfo == null) return;
        dbHelper.deleteAccountInfo(accountInfo);
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

    /**
     * request Account info from server
     */
    public void requestAccountInfo(Handler handler) {
        Account act = getCurrentAccount();
        Thread t = new Thread(new RequestAccountInfoTask(act, handler));
        t.start();
    }

    /**
     * automatically update Account info, like space usage, total space size, from background.
     */
     class RequestAccountInfoTask implements Runnable {

        private Account account;
        private Handler handler;

        public RequestAccountInfoTask(Account account, Handler handler) {
            this.account = account;
            this.handler = handler;
        }

        @Override
        public void run() {
            doRequestAccountInfo(account, handler);

        }
    }
}
