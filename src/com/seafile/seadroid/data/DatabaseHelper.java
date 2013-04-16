package com.seafile.seadroid.data;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.seafile.seadroid.SeadroidApplication;
import com.seafile.seadroid.account.Account;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

public class DatabaseHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "DatabaseHelper";
    // If you change the database schema, you must increment the database version.
    public static final int DATABASE_VERSION = 4;
    public static final String DATABASE_NAME = "data.db";

    // FileCache table
    private static final String FILECACHE_TABLE_NAME = "FileCache";

    private static final String FILECACHE_COLUMN_ID = "id";
    private static final String FILECACHE_COLUMN_FILEID = "fileid";
    private static final String FILECACHE_COLUMN_REPO_NAME = "repo_name";
    private static final String FILECACHE_COLUMN_REPO_ID = "repo_id";
    private static final String FILECACHE_COLUMN_PATH = "path";
    private static final String FILECACHE_COLUMN_ACCOUNT = "account";

    // RepoDir table
    private static final String REPODIR_TABLE_NAME = "RepoDir";

    private static final String REPODIR_COLUMN_ID = "id";
    private static final String REPODIR_COLUMN_ACCOUNT = "account";
    private static final String REPODIR_COLUMN_REPO_NAME = "repo_name";
    private static final String REPODIR_COLUMN_REPO_ID = "repo_id";
    private static final String REPODIR_COLUMN_REPO_DIR = "repo_dir";

    private static final String SQL_CREATE_FILECACHE_TABLE =
        "CREATE TABLE " + FILECACHE_TABLE_NAME + " ("
        + FILECACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
        + FILECACHE_COLUMN_FILEID + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_PATH + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_REPO_NAME + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_REPO_ID + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_ACCOUNT + " TEXT NOT NULL);";

    private static final String SQL_CREATE_REPODIR_TABLE =
        "CREATE TABLE " + REPODIR_TABLE_NAME + " ("
        + REPODIR_COLUMN_ID + " INTEGER PRIMARY KEY, "
        + REPODIR_COLUMN_ACCOUNT + " TEXT NOT NULL, "
        + REPODIR_COLUMN_REPO_NAME + " TEXT NOT NULL, "
        + REPODIR_COLUMN_REPO_ID + " TEXT NOT NULL, "
        + REPODIR_COLUMN_REPO_DIR + " TEXT NOT NULL);";

    // Use only single dbHelper to prevent multi-thread issue and db is closed exception
    // Reference http://stackoverflow.com/questions/2493331/what-are-the-best-practices-for-sqlite-on-android
    private static DatabaseHelper dbHelper = null;
    private SQLiteDatabase database = null;

    public static DatabaseHelper getDatabaseHelper() {
        if (dbHelper != null)
            return dbHelper;
        dbHelper = new DatabaseHelper(SeadroidApplication.getAppContext());
        dbHelper.database = dbHelper.getWritableDatabase();
        return dbHelper;
    }

    private DatabaseHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        createFileCacheTable(db);
        createRepoDirTable(db);
    }

    private void createFileCacheTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_FILECACHE_TABLE);
        db.execSQL("CREATE INDEX fileid_index ON " + FILECACHE_TABLE_NAME
                + " (" + FILECACHE_COLUMN_FILEID + ");");
        db.execSQL("CREATE INDEX repoid_index ON " + FILECACHE_TABLE_NAME
                + " (" + FILECACHE_COLUMN_REPO_ID + ");");
        db.execSQL("CREATE INDEX account_index ON " + FILECACHE_TABLE_NAME
                + " (" + FILECACHE_COLUMN_ACCOUNT + ");");
    }

    private void createRepoDirTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_REPODIR_TABLE);

        String sql;
        sql = String.format("CREATE INDEX account_reponame_index ON %s (%s, %s)",
                            REPODIR_TABLE_NAME,
                            REPODIR_COLUMN_ACCOUNT,
                            REPODIR_COLUMN_REPO_NAME);
        db.execSQL(sql);
        sql = String.format("CREATE UNIQUE INDEX account_reponame_repoid_index ON %s (%s, %s, %s)",
                            REPODIR_TABLE_NAME,
                            REPODIR_COLUMN_ACCOUNT,
                            REPODIR_COLUMN_REPO_NAME,
                            REPODIR_COLUMN_REPO_ID);
        db.execSQL(sql);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        // This database is only a cache for online data, so its upgrade policy is
        // to simply to discard the data and start over

        File dir = new File(DataManager.getExternalRootDirectory());
        for (File f : dir.listFiles()) {
            if (f.isFile()) {
                f.delete();
            }
        }

        db.execSQL("DROP TABLE IF EXISTS " + FILECACHE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + REPODIR_TABLE_NAME + ";");
        onCreate(db);
    }

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

    public SeafCachedFile getFileCacheItem(String repoID,
                                           String path, DataManager dataManager) {
        String[] projection = {
                FILECACHE_COLUMN_ID,
                FILECACHE_COLUMN_FILEID,
                FILECACHE_COLUMN_REPO_NAME,
                FILECACHE_COLUMN_REPO_ID,
                FILECACHE_COLUMN_PATH,
                FILECACHE_COLUMN_ACCOUNT
        };

        Cursor c = database.query(
             FILECACHE_TABLE_NAME,
             projection,
             FILECACHE_COLUMN_REPO_ID
             + "=? and " + FILECACHE_COLUMN_PATH + "=?",
             new String[] { repoID, path },
             null,   // don't group the rows
             null,   // don't filter by row groups
             null    // The sort order
        );

        if (c.moveToFirst() == false) {
            c.close();
            return null;
        }

        SeafCachedFile item = cursorToFileCacheItem(c, dataManager);
        c.close();
        return item;
    }

    // XXX: Here we can use SQLite3  "INSERT OR REPLACE" for convience
    public void saveFileCacheItem(SeafCachedFile item, DataManager dataManager) {
        SeafCachedFile old = getFileCacheItem(item.repoID, item.path, dataManager);
        if (old != null) {
            deleteFileCacheItem(old);
        }

        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(FILECACHE_COLUMN_FILEID, item.fileID);
        values.put(FILECACHE_COLUMN_REPO_NAME, item.repoName);
        values.put(FILECACHE_COLUMN_REPO_ID, item.repoID);
        values.put(FILECACHE_COLUMN_PATH, item.path);
        values.put(FILECACHE_COLUMN_ACCOUNT, item.accountSignature);
        
        // Insert the new row, returning the primary key value of the new row
        database.insert(FILECACHE_TABLE_NAME, null, values);
    }

    public void deleteFileCacheItem(SeafCachedFile item) {
        if (item.id != -1) {
            database.delete(FILECACHE_TABLE_NAME,  FILECACHE_COLUMN_ID + "=?",
                    new String[] { String.valueOf(item.id) });
        } else
            database.delete(FILECACHE_TABLE_NAME,  FILECACHE_COLUMN_REPO_ID + "=? and " + FILECACHE_COLUMN_PATH + "=?",
                new String[] { item.repoID, item.path });
    }

    public List<SeafCachedFile> getFileCacheItems(DataManager dataManager) {
        List<SeafCachedFile> files = new ArrayList<SeafCachedFile>();

        String[] projection = {
                FILECACHE_COLUMN_ID,
                FILECACHE_COLUMN_FILEID,
                FILECACHE_COLUMN_REPO_NAME,
                FILECACHE_COLUMN_REPO_ID,
                FILECACHE_COLUMN_PATH,
                FILECACHE_COLUMN_ACCOUNT
        };

        Cursor c = database.query(
             FILECACHE_TABLE_NAME,
             projection,
             FILECACHE_COLUMN_ACCOUNT + "=?",
             new String[] { dataManager.getAccount().getSignature() },
             null,   // don't group the rows
             null,   // don't filter by row groups
             null    // The sort order
        );

        c.moveToFirst();
        while (!c.isAfterLast()) {
            SeafCachedFile item = cursorToFileCacheItem(c, dataManager);
            files.add(item);
            c.moveToNext();
        }

        c.close();
        return files;
    }

    private SeafCachedFile cursorToFileCacheItem(Cursor cursor, DataManager dataManager) {
        SeafCachedFile item = new SeafCachedFile();
        item.id = cursor.getInt(0);
        item.fileID = cursor.getString(1);
        item.repoName = cursor.getString(2);
        item.repoID = cursor.getString(3);
        item.path = cursor.getString(4);
        item.accountSignature = cursor.getString(5);
        item.file = dataManager.getLocalRepoFile(item.repoName, item.repoID, item.path);
        return item;
    }

    /**
     * Return the directory of a repo on external storage.
     */
    public String getRepoDir(Account account, String repoName, String repoID) {
        String[] projection = {
            REPODIR_COLUMN_REPO_DIR
        };

        String selectClause = String.format("%s = ? and %s = ? and %s = ?",
                                            REPODIR_COLUMN_ACCOUNT,
                                            REPODIR_COLUMN_REPO_NAME,
                                            REPODIR_COLUMN_REPO_ID);

        String[] selectArgs = { account.getSignature(), repoName, repoID };


        Cursor cursor = database.query(
            REPODIR_TABLE_NAME,
            projection,
            selectClause,
            selectArgs,
            null,   // don't group the rows
            null,   // don't filter by row groups
            null);  // The sort order

        if (cursor.moveToFirst() == false) {
            cursor.close();
            return null;
        }

        String dir = cursor.getString(0);
        cursor.close();

        return dir;
    }

    /**
     * Tell if a record exists already.
     */
    public boolean repoDirExists(Account account, String repoName) {

        String[] projection = {
            REPODIR_COLUMN_REPO_DIR
        };

        String selectClause = String.format("%s = ? and %s = ?",
                                            REPODIR_COLUMN_ACCOUNT,
                                            REPODIR_COLUMN_REPO_NAME);

        String[] selectArgs = { account.getSignature(), repoName };


        Cursor cursor = database.query(
            REPODIR_TABLE_NAME,
            projection,
            selectClause,
            selectArgs,
            null,   // don't group the rows
            null,   // don't filter by row groups
            null);  // The sort order

        boolean exist;
        if (cursor.moveToFirst() == false) {
            exist = false;
        } else {
            exist = false;
        }

        cursor.close();

        return exist;
    }

    public void saveRepoDirMapping(Account account, String repoName,
                                   String repoID, String dir) {
        String log = String.format("Saving repo dir mapping: account = %s(%s) "
                                   + "repoName = %s"
                                   + "repoID = %s"
                                   + "dir = %s",
                                   account.getEmail(), account.getServerNoProtocol(),
                                   repoName, repoID, dir);

        Log.d(DEBUG_TAG, log);

        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(REPODIR_COLUMN_ACCOUNT, account.getSignature());
        values.put(REPODIR_COLUMN_REPO_NAME, repoName);
        values.put(REPODIR_COLUMN_REPO_ID, repoID);
        values.put(REPODIR_COLUMN_REPO_DIR, dir);

        database.insert(REPODIR_TABLE_NAME, null, values);
    }
}
