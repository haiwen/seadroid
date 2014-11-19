package com.seafile.seadroid2.account;

import java.util.List;

import android.content.Context;
import android.content.SharedPreferences;

public class AccountManager {
    @SuppressWarnings("unused")
    private static String DEBUG_TAG = "AccountManager";

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
}
