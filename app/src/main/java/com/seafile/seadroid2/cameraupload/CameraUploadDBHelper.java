package com.seafile.seadroid2.cameraupload;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.List;
import java.util.regex.Pattern;

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

    // RepositoryCache table
    private static final String REPOCACHE_TABLE_NAME = "RepositoryCache";
    private static final String REPOCACHE_COLUMN_ID = "id";
    private static final String REPOCACHE_COLUMN_BUCKET = "bucket";
    private static final String REPOCACHE_COLUMN_FILE = "file";
    private static final String REPOCACHE_COLUMN_FILE_SIZE = "file_size";

    private static final String SQL_CREATE_REPOCACHE_TABLE =
            "CREATE TABLE " + REPOCACHE_TABLE_NAME + " ("
                    + REPOCACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + REPOCACHE_COLUMN_BUCKET + " TEXT NOT NULL, "
                    + REPOCACHE_COLUMN_FILE + " TEXT NOT NULL, "
                    + REPOCACHE_COLUMN_FILE_SIZE + " BIGINT NOT NULL);";
    private static final String[] repo_projection = {
            REPOCACHE_COLUMN_ID,
            REPOCACHE_COLUMN_BUCKET,
            REPOCACHE_COLUMN_FILE,
            REPOCACHE_COLUMN_FILE_SIZE
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
        createRepoCacheTable(db);
    }

    private void createPhotoCacheTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_PHOTOCACHE_TABLE);
        db.execSQL("CREATE INDEX photo_repoid_index ON " + PHOTOCACHE_TABLE_NAME
                + " (" + PHOTOCACHE_COLUMN_FILE + ");");
        db.execSQL("CREATE INDEX photo_account_index ON " + PHOTOCACHE_TABLE_NAME
                + " (" + PHOTOCACHE_COLUMN_DATE_ADDED + ");");
    }

    private void createRepoCacheTable(SQLiteDatabase db){
        db.execSQL(SQL_CREATE_REPOCACHE_TABLE);
        db.execSQL("CREATE INDEX repo_bucket_index ON " + REPOCACHE_TABLE_NAME
                + " (" + REPOCACHE_COLUMN_BUCKET + ");");
        db.execSQL("CREATE INDEX repo_file_index ON " + REPOCACHE_TABLE_NAME
                + " (" + REPOCACHE_COLUMN_FILE + ");");
        db.execSQL("CREATE INDEX repo_file_size_index ON " + REPOCACHE_TABLE_NAME
                + " (" + REPOCACHE_COLUMN_FILE_SIZE + ");");
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + PHOTOCACHE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + REPOCACHE_TABLE_NAME + ";");
        onCreate(db);
    }

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

    public boolean isUploaded(File file) {
        String path = file.getAbsolutePath();
        long modified = file.lastModified();

        return isUploaded(path, modified);
    }

    public boolean isUploaded(String path, long modified) {
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

        markAsUploaded(path, modified);
    }

    public void markAsUploaded(String path, long modified) {

        ContentValues values = new ContentValues();
        values.put(PHOTOCACHE_COLUMN_FILE, path);
        values.put(PHOTOCACHE_COLUMN_DATE_ADDED, modified);

        database.insert(PHOTOCACHE_TABLE_NAME, null, values);
    }

    public void cleanCache() {
        cleanPhotoCache();
        cleanRepoCache();
    }

    public void cleanPhotoCache() {
        database.delete(PHOTOCACHE_TABLE_NAME, null, null);
    }

    public void cleanRepoCache() {
        database.delete(REPOCACHE_TABLE_NAME, null, null);
    }
    public void saveRepoList(String bucketName, List<SeafDirent> list){
        for (SeafDirent dirent : list) {
            ContentValues values = new ContentValues();
            values.put(REPOCACHE_COLUMN_BUCKET, bucketName);
            values.put(REPOCACHE_COLUMN_FILE, dirent.name);
            values.put(REPOCACHE_COLUMN_FILE_SIZE, dirent.size);
            database.insert(REPOCACHE_TABLE_NAME, null, values);
        }
    }

    public boolean isInRepo(File file){
        String filename = file.getName();
        long filelength = file.length();
        String bucketName = file.getParent();
        bucketName = bucketName.substring(bucketName.lastIndexOf("/")+1);
        return isInRepo(bucketName, filename, filelength);
    }

    public boolean isInRepo(String bucketName, String filename, long filelength){
        String prefix = filename.substring(0, filename.lastIndexOf("."));
        String suffix = filename.substring(filename.lastIndexOf("."));
//        Cursor c = database.query(
//                REPOCACHE_TABLE_NAME,
//                repo_projection,
//                REPOCACHE_COLUMN_BUCKET + " = ? and " + REPOCACHE_COLUMN_FILE + " REGEXP ? and " + REPOCACHE_COLUMN_FILE_SIZE + " = ?",
//                new String[] { bucketName, Pattern.quote(prefix) + "( \\(\\d+\\))?" + Pattern.quote(suffix), Long.toString(filelength) },
//                null,   // don't group the rows
//                null,   // don't filter by row groups
//                null    // The sort order
//        );

        Cursor c = database.query(
                REPOCACHE_TABLE_NAME,
                repo_projection,
                REPOCACHE_COLUMN_BUCKET + " = ? and " + REPOCACHE_COLUMN_FILE + " = ? and " + REPOCACHE_COLUMN_FILE_SIZE + " = ?",
                new String[] { bucketName, filename, Long.toString(filelength) },
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        int count = c.getCount();
        c.close();
        return count > 0;
    }

    public boolean existRepo(String bucketName){
        String[] bucket_projection = new String[]{REPOCACHE_COLUMN_BUCKET};
        Cursor c = database.query(
                REPOCACHE_TABLE_NAME,
                bucket_projection,
                REPOCACHE_COLUMN_BUCKET + " = ? ",
                new String[] { bucketName },
                null,   // don't group the rows
                null,   // don't filter by row groups
                null    // The sort order
        );

        int count = c.getCount();
        c.close();
        return count > 0;
    }
}