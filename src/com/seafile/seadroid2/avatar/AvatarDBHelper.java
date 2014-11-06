package com.seafile.seadroid2.avatar;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.seafile.seadroid2.SeadroidApplication;

public class AvatarDBHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "AvatarDBHelper";

    public static final int DATABASE_VERSION = 1;
    public static final String DATABASE_NAME = "avatar.db";
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

        List<Avatar> avatars = new ArrayList<Avatar>();
        cursor.moveToFirst();
        while (cursor.moveToNext()) {
            Avatar avatar = new Avatar();
            avatar.setSignature(cursor.getString(0));
            avatar.setUrl(cursor.getString(1));
            avatar.setMtime(cursor.getInt(2));
            avatar.setIs_default(cursor.getInt(3) == 1);
            avatars.add(avatar);
        }
        cursor.close();
        return avatars;
    }

    public void saveAvatars(List<Avatar> avatars) {
        for (Avatar avatar : avatars) {
            if (!isAvatarExist(avatar)) {
                ContentValues values = new ContentValues();
                values.put(AVATAR_COLUMN_SIGNATURE, avatar.getSignature());
                values.put(AVATAR_COLUMN_URL, avatar.getUrl());
                values.put(AVATAR_COLUMN_MTIME, avatar.getMtime());
                values.put(AVATAR_COLUMN_IS_DEFAULT, (avatar.isIs_default() ? 1 : 0));
                database.insert(AVATAR_TABLE_NAME, null, values);
            }
        }
    }
    
    // detect duplicate db insert request
    private boolean isAvatarExist(Avatar avatar) {
        Cursor cursor = database.query(
                AVATAR_TABLE_NAME,
                projection,
                AVATAR_COLUMN_SIGNATURE
                + "=? and " + AVATAR_COLUMN_URL + "=?",
                new String[] { avatar.getSignature(), avatar.getUrl()},
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
                );
        cursor.moveToFirst();
        if (cursor.moveToNext()) {
            cursor.close();
            return true;   
        }
        cursor.close();
        return false;
        
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
