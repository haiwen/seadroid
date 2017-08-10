package com.seafile.seadroid2.account;

import android.content.ContentResolver;
import android.content.Context;
import android.content.SharedPreferences;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.preference.PreferenceManager;
import android.util.Log;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.cameraupload.CameraUploadManager;
import com.seafile.seadroid2.data.ServerInfo;

import java.util.List;

/**
 * Legacy code. Only for migrating old account settings to the new android account store.
 *
 * A helper class to manage {@link #DATABASE_NAME} database creation and version management.
 */
public class AccountDBHelper extends SQLiteOpenHelper {
    public static final String DEBUG_TAG = "AccountDBHelper";

    // NOTE: carefully update onUpgrade() when bumping database versions to make
    // sure user data is saved.

    /** version of adding a new table @{link #ACCOUNT_TABLE_NAME} */
    private static final int VER_ADD_NEW_TABLE_ACCOUNT = 1;
    /** version of adding a new table @{link #SERVER_INFO_TABLE_NAME} */
    private static final int VER_ADD_NEW_TABLE_SERVER_INFO = 2;
    /** version of adding a new table @{link #SERVER_INFO_TABLE_NAME} without losing data */
    private static final int VER_ADD_NEW_TABLE_SERVER_INFO_MIGRATION = 3;
    /** version of moving all accounts to the android account store */
    private static final int VER_MIGRATE_TO_ANDROID_ACCOUNT = 4;

    // If you change the database schema, you must increment the database version.
    private static final int DATABASE_VERSION = VER_MIGRATE_TO_ANDROID_ACCOUNT;

    private static final String DATABASE_NAME = "account.db";

    private static final String ACCOUNT_TABLE_NAME = "Account";
    private static final String SERVER_INFO_TABLE_NAME = "ServerInfo";

    // Account
    private static final String ACCOUNT_COLUMN_SERVER = "server";
    private static final String ACCOUNT_COLUMN_EMAIL = "email";
    private static final String ACCOUNT_COLUMN_TOKEN = "token";

    // Server info
    private static final String SERVER_INFO_COLUMN_URL = "url";
    private static final String SERVER_INFO_COLUMN_VERSION = "version";
    private static final String SERVER_INFO_COLUMN_FEATURE = "feature";

    private static final String SQL_CREATE_SERVER_INFO_TABLE =
            "CREATE TABLE " + SERVER_INFO_TABLE_NAME + " ("
                    + SERVER_INFO_COLUMN_URL + " VARCHAR(255) PRIMARY KEY, "
                    + SERVER_INFO_COLUMN_VERSION + " TEXT NOT NULL, "
                    + SERVER_INFO_COLUMN_FEATURE + " TEXT NOT NULL" + ")";

    private static final String SQL_CREATE_ACCOUNT_TABLE =
            "CREATE TABLE " + ACCOUNT_TABLE_NAME + " ("
                    + ACCOUNT_COLUMN_SERVER + " TEXT NOT NULL, "
                    + ACCOUNT_COLUMN_EMAIL + " TEXT NOT NULL, "
                    + ACCOUNT_COLUMN_TOKEN + " TEXT NOT NULL);";

    private android.accounts.AccountManager mAccountManager;
    private Context context;

    public static void migrateAccounts(Context context) {
        AccountDBHelper db = null;
        try {
            db = new AccountDBHelper(context);
            db.getWritableDatabase().close();  // this forces an onUpgrade()
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private AccountDBHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
        this.context = context;
        mAccountManager = android.accounts.AccountManager.get(context);
    }

    public void onCreate(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_ACCOUNT_TABLE);
        db.execSQL(SQL_CREATE_SERVER_INFO_TABLE);
    }

    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        Log.d(DEBUG_TAG, "onUpgrade() from " + oldVersion + " to " + newVersion);

        // NOTE: This switch statement is designed to handle cascading database
        // updates, starting at the current version and falling through to all
        // future upgrade cases. Only use "break;" when you want to drop and
        // recreate the entire database.

        // Current DB version. We update this variable as we perform upgrades to reflect
        // the current version we are in.
        int version = oldVersion;

        switch (version) {
            case VER_ADD_NEW_TABLE_ACCOUNT:
                // Version 2 added new table "ServerInfo"
                db.execSQL("DROP TABLE IF EXISTS " + SERVER_INFO_TABLE_NAME + ";");
                db.execSQL(SQL_CREATE_SERVER_INFO_TABLE);
                version = VER_ADD_NEW_TABLE_SERVER_INFO;

            case VER_ADD_NEW_TABLE_SERVER_INFO:
                // Version 3 added new table "ServerInfo" without losing user data
                db.execSQL("DROP TABLE IF EXISTS " + SERVER_INFO_TABLE_NAME + ";");
                db.execSQL(SQL_CREATE_SERVER_INFO_TABLE);
                version = VER_ADD_NEW_TABLE_SERVER_INFO_MIGRATION;
            case VER_ADD_NEW_TABLE_SERVER_INFO_MIGRATION:
                migrateToAndroidAccount(db);
                db.execSQL("DROP TABLE IF EXISTS " + ACCOUNT_TABLE_NAME + ";");
                db.execSQL("DROP TABLE IF EXISTS " + SERVER_INFO_TABLE_NAME + ";");
                version = VER_MIGRATE_TO_ANDROID_ACCOUNT;
        }

        Log.d(DEBUG_TAG, "after upgrade logic, at version " + version);

        // at this point, we ran out of upgrade logic, so if we are still at the wrong
        // version, we have no choice but to delete everything and create everything again.
        if (version != DATABASE_VERSION) {
            Log.w(DEBUG_TAG, "Destroying old data during upgrade");
            db.execSQL("DROP TABLE IF EXISTS " + ACCOUNT_TABLE_NAME + ";");
            db.execSQL("DROP TABLE IF EXISTS " + SERVER_INFO_TABLE_NAME + ";");
            onCreate(db);
            version = DATABASE_VERSION;
        }
    }

    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

    private void migrateToAndroidAccount(SQLiteDatabase db) {

        Log.i(DEBUG_TAG, "Migrating seafile accounts into Android account store (upgrade)");

        SharedPreferences sharedPref = context.getSharedPreferences(AccountManager.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        SharedPreferences settingsSharedPref = PreferenceManager.getDefaultSharedPreferences(context);

        Account cameraAccount = null;
        String cameraServer = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        String cameraEmail = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);
        String cameraToken = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, null);
        if (settingsSharedPref.getBoolean(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY, false) && cameraEmail != null
                && cameraServer != null && cameraToken != null) {

            // on this account camera upload was done previously
            cameraAccount = new Account(cameraServer, cameraEmail, cameraToken, false);
        }

        for (Account account: getAccountList(db)) {
            Log.d(DEBUG_TAG, "Migrating seafile account: " + account);

            // MIGRATE account
            Log.d(DEBUG_TAG, "adding account: " + account);
            mAccountManager.addAccountExplicitly(account.getAndroidAccount(), null, null);
            mAccountManager.setAuthToken(account.getAndroidAccount(), Authenticator.AUTHTOKEN_TYPE, account.getToken());
            mAccountManager.setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_URI, account.getServer());
            mAccountManager.setUserData(account.getAndroidAccount(), Authenticator.KEY_EMAIL, account.getEmail());

            // MIGRATE ServerInfo
            ServerInfo info = getServerInfo(db, account.getServer());
            if (info != null) {
                Log.d(DEBUG_TAG, "setting server info: " + info);
                mAccountManager.setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_FEATURES, info.getFeatures());
                mAccountManager.setUserData(account.getAndroidAccount(), Authenticator.KEY_SERVER_VERSION, info.getVersion());
            }

            // MIGRATE camera sync settings
            if (cameraAccount != null && cameraAccount.equals(account)) {
                Log.d(DEBUG_TAG, "enabling camera sync");
                ContentResolver.setIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, 1);
                ContentResolver.setSyncAutomatically(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, true);
            } else {
                ContentResolver.setIsSyncable(account.getAndroidAccount(), CameraUploadManager.AUTHORITY, 0);
            }
            Log.d(DEBUG_TAG, "Finished migrating seafile account: " + account);
        }

        Log.i(DEBUG_TAG, "Finished migration of seafile accounts");
    }

    private List<Account> getAccountList(SQLiteDatabase database) {
        List<Account> accounts = Lists.newArrayList();

        String[] projection = {
                AccountDBHelper.ACCOUNT_COLUMN_SERVER,
                AccountDBHelper.ACCOUNT_COLUMN_EMAIL,
                AccountDBHelper.ACCOUNT_COLUMN_TOKEN
        };

        Cursor c = database.query(
             AccountDBHelper.ACCOUNT_TABLE_NAME,
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
        return accounts;
    }

    private Account cursorToAccount(Cursor cursor) {
        return new Account(cursor.getString(0), cursor.getString(1), cursor.getString(2), false);
    }

    private ServerInfo getServerInfo(SQLiteDatabase database, String url) {
        String[] projection = {SERVER_INFO_COLUMN_URL, SERVER_INFO_COLUMN_VERSION, SERVER_INFO_COLUMN_FEATURE};

        Cursor c = database.query(SERVER_INFO_TABLE_NAME,
                projection,
                "url=?",
                new String[] {url},
                null,  // don't group the rows
                null,  // don't filter by row groups
                null); // The sort order

        if (!c.moveToFirst()) {
            c.close();
            return null;
        }

        ServerInfo serverInfo = cursorToServerInfo(c);

        c.close();
        return serverInfo;
    }

    private ServerInfo cursorToServerInfo(Cursor cursor) {
        String url = cursor.getString(0);
        String version = cursor.getString(1);
        String features = cursor.getString(2);
        ServerInfo serverInfo = new ServerInfo(url, version, features);
        return serverInfo;
    }

}
