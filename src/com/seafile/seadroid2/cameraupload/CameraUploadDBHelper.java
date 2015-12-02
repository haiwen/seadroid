package com.seafile.seadroid2.cameraupload;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.DatabaseUtils;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.avatar.Avatar;
import com.seafile.seadroid2.data.SeafCachedPhoto;

import java.util.List;

public class CameraUploadDBHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "CameraUploadDBHelper";

    // If you change the database schema, you must increment the database
    // version.
    public static final int DATABASE_VERSION = 2;
    public static final String DATABASE_NAME = "photo.db";
    private static CameraUploadDBHelper dbHelper;
    private SQLiteDatabase database;

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

    // Photo directories table
    public static final String PHOTO_DIRECTORIES_TABLE_NAME = "PhotoDirectories";
    public static final String PHOTO_DIRECTORIES_ID = "id";
    public static final String PHOTO_DIRECTORIES_PATH = "directory_path";
    public static final String PHOTO_DIRECTORIES_INCLUDE = "include";

    private static final String SQL_CREATE_PHOTO_DIRECTORIES_TABLE =
            "CREATE TABLE " + PHOTO_DIRECTORIES_TABLE_NAME + " ("
                    + PHOTO_DIRECTORIES_ID + " INTEGER PRIMARY KEY, "
                    + PHOTO_DIRECTORIES_PATH + " TEXT NOT NULL, "
                    + PHOTO_DIRECTORIES_INCLUDE + " TEXT NOT NULL);";

    private static final String[] directoryProjection = {PHOTO_DIRECTORIES_PATH};

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
        createPhotoDirectoriesTable(db);
    }

    private void createPhotoCacheTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_PHOTOCACHE_TABLE);
        db.execSQL("CREATE INDEX photo_repoid_index ON " + PHOTOCACHE_TABLE_NAME
                + " (" + PHOTOCACHE_COLUMN_REPO_ID + ");");
        db.execSQL("CREATE INDEX photo_account_index ON " + PHOTOCACHE_TABLE_NAME
                + " (" + PHOTOCACHE_COLUMN_ACCOUNT + ");");
    }

    private void createPhotoDirectoriesTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_PHOTO_DIRECTORIES_TABLE);
        db.execSQL("CREATE INDEX directory_path_index ON " + PHOTO_DIRECTORIES_TABLE_NAME
                + " (" + PHOTO_DIRECTORIES_PATH + ");");
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + PHOTOCACHE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + PHOTO_DIRECTORIES_TABLE_NAME + ";");
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
            return database.delete(PHOTOCACHE_TABLE_NAME, PHOTOCACHE_COLUMN_ID + "=?",
                    new String[]{String.valueOf(item.id)});
        } else
            return database.delete(PHOTOCACHE_TABLE_NAME, PHOTOCACHE_COLUMN_REPO_ID + "=? and " + PHOTOCACHE_COLUMN_PATH + "=?",
                    new String[]{item.repoID, item.path});
    }

    // avoid duplicate inserting request
    private boolean isRowDuplicate(String path) {

        long count = DatabaseUtils.queryNumEntries(
                database,
                PHOTO_DIRECTORIES_TABLE_NAME,
                PHOTO_DIRECTORIES_PATH + "=?",
                new String[]{path});
        return count > 0;

    }

    public void removeDirList(List<String> toRemoveList) {
        for (String toRemove : toRemoveList) {
            database.delete(PHOTO_DIRECTORIES_TABLE_NAME, PHOTO_DIRECTORIES_PATH + "=?",
                    new String[]{toRemove});
        }
    }

    public void saveSelectedDirectory(String path, boolean isChecked) {

        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(PHOTO_DIRECTORIES_PATH, path);
        values.put(PHOTO_DIRECTORIES_INCLUDE, isChecked);

        if (!isRowDuplicate(path)) {
            // Insert the new row, returning the primary key value of the new row
            database.replace(PHOTO_DIRECTORIES_TABLE_NAME, null, values);
        } else
            database.update(PHOTO_DIRECTORIES_TABLE_NAME, values,
                    PHOTO_DIRECTORIES_PATH + "=?",
                    new String[]{path});
    }

    /**
     * Returns all photo directory paths in the table.
     */
    public List<String> getCustomDirList() {
        Cursor cursor = database.query(
                PHOTO_DIRECTORIES_TABLE_NAME,
                directoryProjection,
                PHOTO_DIRECTORIES_INCLUDE + "=?",
                new String[] {"1"},
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        List<String> directories = Lists.newArrayList();

        if (!cursor.moveToFirst())
            return directories;

        do {
            directories.add(cursor.getString(0)); // folder path column
        } while (cursor.moveToNext());

        cursor.close();
        return directories;
    }
}
