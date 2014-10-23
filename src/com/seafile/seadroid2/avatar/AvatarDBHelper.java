package com.seafile.seadroid2.avatar;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

import com.seafile.seadroid2.SeadroidApplication;

public class AvatarDBHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "AvatarDbHelper";

    // If you change the database schema, you must increment the database version.
    public static final int DATABASE_VERSION = 1;
    public static final String DATABASE_NAME = "avatar.db";
    
    // Use only single dbHelper to prevent multi-thread issue and db is closed exception
    // Reference
    // http://stackoverflow.com/questions/2493331/what-are-the-best-practices-for-sqlite-on-android
    private static AvatarDBHelper dbHelper = null;
    private SQLiteDatabase database = null;

    public static synchronized AvatarDBHelper getAvatarDbHelper() {
        if (dbHelper != null)
            return dbHelper;
        dbHelper = new AvatarDBHelper(SeadroidApplication.getAppContext());
        dbHelper.database = dbHelper.getWritableDatabase();
        return dbHelper;
    }

    private AvatarDBHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    // Avatar table
    private static final String AVATAR_TABLE_NAME = "Avatar";
    
    private static final String AVATAR_COLUMN_ID = "id";
    private static final String AVATAR_COLUMN_SIGNATURE = "signature";
    private static final String AVATAR_COLUMN_URL = "url";
    private static final String AVATAR_COLUMN_MTIME = "mtime";
    private static final String AVATAR_COLUMN_IS_DEFAULT = "is_default";

    private static final String SQL_CREATE_AVATAR_TABLE =
            "CREATE TABLE " + AVATAR_TABLE_NAME + " ("
            + AVATAR_COLUMN_ID + " INTEGER PRIMARY KEY, "
            + AVATAR_COLUMN_SIGNATURE + " TEXT NOT NULL, "
            + AVATAR_COLUMN_URL + " TEXT NOT NULL, "
            + AVATAR_COLUMN_MTIME + " INTEGER NOT NULL, "
            + AVATAR_COLUMN_IS_DEFAULT + " INTEGER NOT NULL);";

    private static final String[] projection = {
            //AVATAR_COLUMN_ID,
            AVATAR_COLUMN_SIGNATURE,
            AVATAR_COLUMN_URL,
            AVATAR_COLUMN_MTIME,
            AVATAR_COLUMN_IS_DEFAULT
            };
    
    public List<Avatar> getAvatarList() {
        
        Cursor cursor = database.query(
        AVATAR_TABLE_NAME,
        projection,
        null,
        null,
        null,   // don't group the rows
        null,   // don't filter by row groups
        null    // The sort order
        );

        /*if (!c.moveToFirst()) {
            c.close();
            return null;
        }
        */
        List<Avatar> avatars = new ArrayList<Avatar>();
        cursor.moveToFirst();
        if (!cursor.isAfterLast()) {
            do {
                Avatar avatar = new Avatar();
                avatar.setSignature(cursor.getString(0));
                avatar.setUrl(cursor.getString(1));
                avatar.setMtime(cursor.getInt(2));
                avatar.setIs_default(cursor.getInt(3) == 1);
                avatars.add(avatar);
            } while (cursor.moveToNext());
        }
        /*while (cursor.moveToNext()) {
            
        }*/
        cursor.close();
        return avatars;
    }

    public void saveAvatars(List<Avatar> avatars) {
        Log.d(DEBUG_TAG, "saveAvatar to db");
        for (Avatar avatar : avatars) {
            // Create a new map of values, where column names are the keys
            ContentValues values = new ContentValues();
            values.put(AVATAR_COLUMN_SIGNATURE, avatar.getSignature());
            values.put(AVATAR_COLUMN_URL, avatar.getUrl());
            values.put(AVATAR_COLUMN_MTIME, avatar.getMtime());
            values.put(AVATAR_COLUMN_IS_DEFAULT, (avatar.isIs_default() ? 1 : 0));
            // Insert the new row, returning the primary key value of the new
            // row
            Long rlt = database.insert(AVATAR_TABLE_NAME, null, values);
            Log.d(DEBUG_TAG, "insert: " + rlt);
        }
    }
    
    @Override
    public void onCreate(SQLiteDatabase db) {
        createAvatarTable(db);
    }

    private void createAvatarTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_AVATAR_TABLE);
        db.execSQL("CREATE INDEX account_signature_index ON " + AVATAR_TABLE_NAME
                + " (" + AVATAR_COLUMN_SIGNATURE + ");");
        db.execSQL("CREATE INDEX avatar_url_index ON " + AVATAR_TABLE_NAME
                + " (" + AVATAR_COLUMN_URL + ");");
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + AVATAR_TABLE_NAME + ";");
        onCreate(db);
    }

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

}
