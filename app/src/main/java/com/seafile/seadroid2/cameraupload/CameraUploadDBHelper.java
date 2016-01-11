package com.seafile.seadroid2.cameraupload;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.seafile.seadroid2.SeadroidApplication;

import java.io.File;

public class CameraUploadDBHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "CameraUploadDBHelper";

    // If you change the database schema, you must increment the database
    // version.
    public static final int DATABASE_VERSION = 3;
    public static final String DATABASE_NAME = "photo.db";
    private static CameraUploadDBHelper dbHelper;
    private SQLiteDatabase database;

    // PhotoCache table
    private static final String PHOTOCACHE_TABLE_NAME = "PhotoCache";
    private static final String PHOTOCACHE_COLUMN_ID = "id";
    private static final String PHOTOCACHE_COLUMN_FILE = "file";
    private static final String PHOTOCACHE_COLUMN_DATE_ADDED = "date_added";

    private static final String SQL_CREATE_PHOTOCACHE_TABLE =
            "CREATE TABLE " + PHOTOCACHE_TABLE_NAME + " ("
                    + PHOTOCACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + PHOTOCACHE_COLUMN_FILE + " TEXT NOT NULL, "
                    + PHOTOCACHE_COLUMN_DATE_ADDED + " BIGINT NOT NULL);";

    private static final String[] projection = {
            PHOTOCACHE_COLUMN_ID,
            PHOTOCACHE_COLUMN_FILE,
            PHOTOCACHE_COLUMN_DATE_ADDED
    };

    public static synchronized CameraUploadDBHelper getInstance() {
        if (dbHelper == null) {
            dbHelper = new CameraUploadDBHelper(SeadroidApplication.getAppContext());
            dbHelper.database = dbHelper.getWritableDatabase();
        }

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
                + " (" + PHOTOCACHE_COLUMN_FILE + ");");
        db.execSQL("CREATE INDEX photo_account_index ON " + PHOTOCACHE_TABLE_NAME
                + " (" + PHOTOCACHE_COLUMN_DATE_ADDED + ");");
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

    public boolean isUploaded(File file) {
        String path = file.getAbsolutePath();
        long modified = file.lastModified();

        Cursor c = database.query(
                PHOTOCACHE_TABLE_NAME,
                projection,
                PHOTOCACHE_COLUMN_FILE + " = ? and " + PHOTOCACHE_COLUMN_DATE_ADDED + " = ?",
                new String[] { path, Long.toString(modified) },
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        int count = c.getCount();
        c.close();
        return count > 0;
    }

    public void markAsUploaded(File file) {
        String path = file.getAbsolutePath();
        long modified = file.lastModified();

        ContentValues values = new ContentValues();
        values.put(PHOTOCACHE_COLUMN_FILE, path);
        values.put(PHOTOCACHE_COLUMN_DATE_ADDED, modified);

        database.insert(PHOTOCACHE_TABLE_NAME, null, values);
    }

    public void cleanPhotoCache() {
        database.delete(PHOTOCACHE_TABLE_NAME, null, null);
    }
}