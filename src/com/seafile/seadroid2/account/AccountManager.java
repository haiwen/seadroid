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

    private final AccountDBHelper dbHelper;
    private Context context;

    public AccountManager(Context context) {
        this.context = context;
        dbHelper = AccountDBHelper.getDatabaseHelper(context);
    }

    public Account getDefaultAccount() {
        SharedPreferences settings = context.getSharedPreferences("Account", 0);
        String defaultServer = settings.getString("server", "");
        String defaultEmail = settings.getString("email", "");

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
        SharedPreferences sharedPref = context.getSharedPreferences("Account", 0);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString("server", account.server);
        editor.putString("email", account.email);
        editor.commit();

        // save to db
        dbHelper.saveAccount(account);
    }

    public void updateAccount(Account oldAccount, Account newAccount) {
        // save to shared preference
        SharedPreferences sharedPref = context.getSharedPreferences("Account", 0);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString("server", newAccount.server);
        editor.putString("email", newAccount.email);
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

    /**
     * delete token of the current Account instance.<br>
     * If Camera Upload Service is running under the Account, stop the service.
     *
     * @param account
     */
    public void deleteTokenByAccount(Account account) {
        SharedPreferences sharedPref = context.getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();

        String latestServer = sharedPref.getString(AccountsActivity.SHARED_PREF_SERVER_KEY, null);
        String latestEmail = sharedPref.getString(AccountsActivity.SHARED_PREF_EMAIL_KEY, null);

        String signOutServer = account.getServer();
        String signOutEmail = account.getEmail();

        if (signOutServer.equals(latestServer) && signOutEmail.equals(latestEmail)) {
            // delete token
            editor.putString(AccountsActivity.SHARED_PREF_TOKEN_KEY, null);
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
        SharedPreferences spf = context.getSharedPreferences("Account", Context.MODE_PRIVATE);
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
            SharedPreferences sharedPref = context.getSharedPreferences("Account", Context.MODE_PRIVATE);
            SharedPreferences.Editor editor = sharedPref.edit();
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
