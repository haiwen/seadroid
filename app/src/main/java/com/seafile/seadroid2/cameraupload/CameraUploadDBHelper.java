package com.seafile.seadroid2.cameraupload;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.SeafCachedPhoto;

public class CameraUploadDBHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "CameraUploadDBHelper";

    // If you change the database schema, you must increment the database
    // version.
    public static final int DATABASE_VERSION = 1;
    public static final String DATABASE_NAME = "photo.db";

    // PhotoCache table
    private static final String PHOTOCACHE_TABLE_NAME = "PhotoCache";

    private static final String PHOTOCACHE_COLUMN_ID = "id";
    private static final String PHOTOCACHE_COLUMN_REPO_NAME = "repo_name";
    private static final String PHOTOCACHE_COLUMN_REPO_ID = "repo_id";
    private static final String PHOTOCACHE_COLUMN_PATH = "path";
    private static final String PHOTOCACHE_COLUMN_ACCOUNT = "account";

    private static final String SQL_CREATE_PHOTOCACHE_TABLE =
            "CREATE TABLE " + PHOTOCACHE_TABLE_NAME + " ("
            + PHOTOCACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
            + PHOTOCACHE_COLUMN_PATH + " TEXT NOT NULL, "
            + PHOTOCACHE_COLUMN_REPO_NAME + " TEXT NOT NULL, "
            + PHOTOCACHE_COLUMN_REPO_ID + " TEXT NOT NULL, "
            + PHOTOCACHE_COLUMN_ACCOUNT + " TEXT NOT NULL);";

    private static final String[] projection = {
            PHOTOCACHE_COLUMN_ID,
            PHOTOCACHE_COLUMN_REPO_NAME,
            PHOTOCACHE_COLUMN_REPO_ID,
            PHOTOCACHE_COLUMN_PATH,
            PHOTOCACHE_COLUMN_ACCOUNT
            };

    // Use only single dbHelper to prevent multi-thread issue and db is closed exception
    // Reference
    // http://stackoverflow.com/questions/2493331/what-are-the-best-practices-for-sqlite-on-android
    private static CameraUploadDBHelper dbHelper = null;
    private SQLiteDatabase database = null;

    public static synchronized CameraUploadDBHelper getCameraUploadDBHelper() {
        if (dbHelper != null)
            return dbHelper;
        dbHelper = new CameraUploadDBHelper(SeadroidApplication.getAppContext());
        dbHelper.database = dbHelper.getWritableDatabase();
        return dbHelper;
    }

    private CameraUploadDBHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        createPhotoCacheTable(db);
    }

    private void createPhotoCacheTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_PHOTOCACHE_TABLE);
        db.execSQL("CREATE INDEX photo_repoid_index ON " + PHOTOCACHE_TABLE_NAME
                + " (" + PHOTOCACHE_COLUMN_REPO_ID + ");");
        db.execSQL("CREATE INDEX photo_account_index ON " + PHOTOCACHE_TABLE_NAME
                + " (" + PHOTOCACHE_COLUMN_ACCOUNT + ");");
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + PHOTOCACHE_TABLE_NAME + ";");
        onCreate(db);
    }

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

    public SeafCachedPhoto getPhotoCacheItem(String repoID,
            String path) {
        Cursor c = database.query(
        PHOTOCACHE_TABLE_NAME,
        projection,
        PHOTOCACHE_COLUMN_REPO_ID
        + "=? and " + PHOTOCACHE_COLUMN_PATH + "=?",
        new String[] { repoID, path },
        null,   // don't group the rows
        null,   // don't filter by row groups
        null    // The sort order
        );

        if (!c.moveToFirst()) {
            c.close();
            return null;
        }

        SeafCachedPhoto item = new SeafCachedPhoto();
        item.id = c.getInt(0);
        item.repoName = c.getString(1);
        item.repoID = c.getString(2);
        item.path = c.getString(3);
        item.accountSignature = c.getString(4);
        c.close();
        return item;
    }

    public void savePhotoCacheItem(SeafCachedPhoto item) {

        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(PHOTOCACHE_COLUMN_REPO_NAME, item.repoName);
        values.put(PHOTOCACHE_COLUMN_REPO_ID, item.repoID);
        values.put(PHOTOCACHE_COLUMN_PATH, item.path);
        values.put(PHOTOCACHE_COLUMN_ACCOUNT, item.accountSignature);

        // Insert the new row, returning the primary key value of the new row
        database.insert(PHOTOCACHE_TABLE_NAME, null, values);
    }

    public int removePhotoCacheItem(SeafCachedPhoto item) {
        if (item.id != -1) {
            return database.delete(PHOTOCACHE_TABLE_NAME,  PHOTOCACHE_COLUMN_ID + "=?",
                    new String[] { String.valueOf(item.id) });
        } else
            return database.delete(PHOTOCACHE_TABLE_NAME,  PHOTOCACHE_COLUMN_REPO_ID + "=? and " + PHOTOCACHE_COLUMN_PATH + "=?",
                new String[] { item.repoID, item.path });
    }
}
