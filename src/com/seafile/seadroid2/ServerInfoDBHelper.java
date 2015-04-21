package com.seafile.seadroid2;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;
import com.seafile.seadroid2.data.ServerInfo;

/**
 * Database of server info
 */
public class ServerInfoDBHelper extends SQLiteOpenHelper {
    // If you change the database schema, you must increment the database version.
    private static final int DATABASE_VERSION = 1;
    private static final String DATABASE_NAME = "server.db";

    private static final String TABLE_NAME = "ServerInfo";

    private static final String COLUMN_URL = "url";
    public static final String COLUMN_FEATURE = "feature";
    private static final String COLUMN_VERSION = "version";

    private static final String CREATE_TABLE_SQL = "CREATE TABLE " + TABLE_NAME + " ("
            + COLUMN_URL + " VARCHAR(255) PRIMARY KEY, " + COLUMN_VERSION + " TEXT, " + COLUMN_FEATURE + " TEXT " + ")";

    private static ServerInfoDBHelper instance = null;
    private SQLiteDatabase database = null;

    public static synchronized ServerInfoDBHelper getInstance() {
        if (instance != null)
            return instance;
        instance = new ServerInfoDBHelper(SeadroidApplication.getAppContext());
        instance.database = instance.getWritableDatabase();
        return instance;
    }

    private ServerInfoDBHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        db.execSQL(CREATE_TABLE_SQL);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {}

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {}

    public ServerInfo getServerInfo(String url) {
        if (url == null) return null;

        String[] projection = {COLUMN_URL, COLUMN_VERSION, COLUMN_FEATURE};

        Cursor c = database.query(TABLE_NAME,
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
        serverInfo.setProEdition(features.contains("seafile-pro"));
        return serverInfo;
    }

    public void saveServerInfo(ServerInfo serverInfo) {
        ContentValues values = new ContentValues();
        values.put(COLUMN_URL, serverInfo.getUrl());
        values.put(COLUMN_VERSION, serverInfo.getVersion());
        values.put(COLUMN_FEATURE, serverInfo.getFeatures());

        database.replace(TABLE_NAME, null, values);
    }
}