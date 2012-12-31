package com.seafile.seadroid.account;

import java.util.ArrayList;
import java.util.List;


import android.content.ContentValues;
import android.content.Context;
import android.content.SharedPreferences;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.util.Log;


public class AccountManager
{   
    private static String DEBUG_TAG = "AccountManager";
    
    AccountDbHelper dbHelper;
    Context context;

    public AccountManager(Context context) {
        dbHelper = new AccountDbHelper(context);
        this.context = context;
    }

    public void login() {

    }
    
    public void getAccount(String server) {
        
    }
    
    public Account getDefaultAccount() {
        SharedPreferences settings = context.getSharedPreferences("Account", 0);
        String defaultServer = settings.getString("server", "");
        String defaultEmail = settings.getString("email", "");
        
        if (defaultServer.length() == 0 || defaultEmail.length() == 0)
            return null;
                    
        return getAccount(defaultServer, defaultEmail);
    }
    
    public Account getAccount(String server, String email) {
        SQLiteDatabase db = dbHelper.getReadableDatabase();

        String[] projection = {          
                AccountDbHelper.COLUMN_SERVER,
                AccountDbHelper.COLUMN_EMAIL,
                AccountDbHelper.COLUMN_TOKEN
        };

        Cursor c = db.query(
             AccountDbHelper.TABLE_NAME,
             projection,
             "server=? and email=?",
             new String[] { server, email },                                     
             null,   // don't group the rows
             null,   // don't filter by row groups
             null    // The sort order
         );
     
        if (c.moveToFirst() == false) {
            c.close();
            db.close();
            return null;
        }

        Account account = cursorToAccount(c);
        c.close();
        db.close();
        return account;
    }
    
    public void saveAccount(Account account) {
        Account old = getAccount(account.server, account.email);
        if (old != null) {
            if (old.token.equals(account.token))
                return;
            else
                deleteAccount(old);
        }
        
        // Gets the data repository in write mode
        SQLiteDatabase db = dbHelper.getWritableDatabase();
        
        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(AccountDbHelper.COLUMN_SERVER, account.server);
        values.put(AccountDbHelper.COLUMN_EMAIL, account.email);
        values.put(AccountDbHelper.COLUMN_TOKEN, account.token);

        // Insert the new row, returning the primary key value of the new row
        db.insert(AccountDbHelper.TABLE_NAME, null, values);
        db.close();
    }
    
    public void saveDefaultAccount(Account account) {
        // save to shared preference
        SharedPreferences sharedPref = context.getSharedPreferences("Account", 0);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString("server", account.server);
        editor.putString("email", account.email);
        editor.commit();
        
        // save to db
        saveAccount(account);
    }
    
    public void deleteAccount(Account account) {
        // Gets the data repository in write mode
        SQLiteDatabase db = dbHelper.getWritableDatabase();

        // Insert the new row, returning the primary key value of the new row
        db.delete(AccountDbHelper.TABLE_NAME,  "server=? and email=?",
                new String[] { account.server, account.email });
        db.close();
    }
    
    public List<Account> getAccountList() {
        List<Account> accounts = new ArrayList<Account>();
        
        SQLiteDatabase db = dbHelper.getReadableDatabase();

        String[] projection = {          
                AccountDbHelper.COLUMN_SERVER,
                AccountDbHelper.COLUMN_EMAIL,
                AccountDbHelper.COLUMN_TOKEN
        };

        Cursor c = db.query(
             AccountDbHelper.TABLE_NAME,
             projection,
             null,
             null,                                     
             null,   // don't group the rows
             null,   // don't filter by row groups
             null    // The sort order
        );
        
        c.moveToFirst();
        while (!c.isAfterLast()) {
            Account account = cursorToAccount(c);
            accounts.add(account);
            c.moveToNext();
        }
        
        c.close();
        db.close();
        return accounts;
    }
    
    public  Account getDemoAccout() {
        return new Account("http://cloud.seafile.com", "demo@seafile.com", "demo", null);
    }
    
    private Account cursorToAccount(Cursor cursor) {
        Account account = new Account();
        account.server = cursor.getString(0);
        account.email = cursor.getString(1);
        account.token = cursor.getString(2);
        return account;
    }

}
