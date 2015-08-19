package com.horizonbase.seadroid2.data;

import java.io.File;
import java.util.List;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;
import android.util.Pair;

import com.google.common.collect.Lists;
import com.horizonbase.seadroid2.SeadroidApplication;
import com.horizonbase.seadroid2.account.Account;

public class DatabaseHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "DatabaseHelper";
    // If you change the database schema, you must increment the database version.
    public static final int DATABASE_VERSION = 6;
    public static final String DATABASE_NAME = "data.db";

    // FileCache table
    private static final String FILECACHE_TABLE_NAME = "FileCache";

    private static final String FILECACHE_COLUMN_ID = "id";
    private static final String FILECACHE_COLUMN_FILEID = "fileid";
    private static final String FILECACHE_COLUMN_REPO_NAME = "repo_name";
    private static final String FILECACHE_COLUMN_REPO_ID = "repo_id";
    private static final String FILECACHE_COLUMN_PATH = "path";
    private static final String FILECACHE_COLUMN_ACCOUNT = "account";

    private static final String STARRED_FILECACHE_TABLE_NAME = "StarredFileCache";

    private static final String STARRED_FILECACHE_COLUMN_ID = "id";
    private static final String STARRED_FILECACHE_COLUMN_ACCOUNT = "account";
    private static final String STARRED_FILECACHE_COLUMN_CONTENT = "content";

    // RepoDir table
    private static final String REPODIR_TABLE_NAME = "RepoDir";

    private static final String REPODIR_COLUMN_ID = "id";
    private static final String REPODIR_COLUMN_ACCOUNT = "account";
    private static final String REPODIR_COLUMN_REPO_NAME = "repo_name";
    private static final String REPODIR_COLUMN_REPO_ID = "repo_id";
    private static final String REPODIR_COLUMN_REPO_DIR = "repo_dir";

    private static final String DIRENTS_CACHE_TABLE_NAME = "DirentsCache";

    private static final String DIRENTS_CACHE_COLUMN_ID = "id";
    private static final String DIRENTS_CACHE_COLUMN_REPO_ID = "repo_id";
    private static final String DIRENTS_CACHE_COLUMN_PATH = "path";
    private static final String DIRENTS_CACHE_COLUMN_DIR_ID = "dir_id";
    private static final String DIRENTS_CACHE_COLUMN_CONTENT = "content";

    private static final String SQL_CREATE_FILECACHE_TABLE =
        "CREATE TABLE " + FILECACHE_TABLE_NAME + " ("
        + FILECACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
        + FILECACHE_COLUMN_FILEID + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_PATH + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_REPO_NAME + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_REPO_ID + " TEXT NOT NULL, "
        + FILECACHE_COLUMN_ACCOUNT + " TEXT NOT NULL);";

    private static final String SQL_CREATE_STARRED_FILECACHE_TABLE =
            "CREATE TABLE " + STARRED_FILECACHE_TABLE_NAME + " ("
                    + STARRED_FILECACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + STARRED_FILECACHE_COLUMN_ACCOUNT + " TEXT NOT NULL, "
                    + STARRED_FILECACHE_COLUMN_CONTENT + " TEXT NOT NULL);";

    private static final String SQL_CREATE_REPODIR_TABLE =
        "CREATE TABLE " + REPODIR_TABLE_NAME + " ("
        + REPODIR_COLUMN_ID + " INTEGER PRIMARY KEY, "
        + REPODIR_COLUMN_ACCOUNT + " TEXT NOT NULL, "
        + REPODIR_COLUMN_REPO_NAME + " TEXT NOT NULL, "
        + REPODIR_COLUMN_REPO_ID + " TEXT NOT NULL, "
        + REPODIR_COLUMN_REPO_DIR + " TEXT NOT NULL);";

    private static final String SQL_CREATE_DIRENTS_CACHE_TABLE =
        "CREATE TABLE " + DIRENTS_CACHE_TABLE_NAME + " ("
        + DIRENTS_CACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
        + DIRENTS_CACHE_COLUMN_REPO_ID + " TEXT NOT NULL, "
        + DIRENTS_CACHE_COLUMN_PATH + " TEXT NOT NULL, "
        + DIRENTS_CACHE_COLUMN_DIR_ID + " TEXT NOT NULL, "
        + DIRENTS_CACHE_COLUMN_CONTENT + " TEXT NOT NULL);";

    // Use only single dbHelper to prevent multi-thread issue and db is closed exception
    // Reference http://stackoverflow.com/questions/2493331/what-are-the-best-practices-for-sqlite-on-android
    private static DatabaseHelper dbHelper = null;
    private SQLiteDatabase database = null;

    public static synchronized DatabaseHelper getDatabaseHelper() {
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
        createDirentsCacheTable(db);
        createStarredFilesCacheTable(db);
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

    private void createDirentsCacheTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_DIRENTS_CACHE_TABLE);

        String sql;
        sql = String.format("CREATE INDEX repo_path_index ON %s (%s, %s)",
                            DIRENTS_CACHE_TABLE_NAME,
                            DIRENTS_CACHE_COLUMN_REPO_ID,
                            DIRENTS_CACHE_COLUMN_PATH);
        db.execSQL(sql);
    }

    private void createStarredFilesCacheTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_STARRED_FILECACHE_TABLE);

        String sql;
        sql = String.format("CREATE INDEX account_content_index ON %s (%s, %s)",
                STARRED_FILECACHE_TABLE_NAME,
                STARRED_FILECACHE_COLUMN_ACCOUNT,
                STARRED_FILECACHE_COLUMN_CONTENT);
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
        db.execSQL("DROP TABLE IF EXISTS " + DIRENTS_CACHE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + STARRED_FILECACHE_TABLE_NAME + ";");
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

        if (!c.moveToFirst()) {
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

    /**
     * delete all cache info under one specific account from database
     *
     * @param account
     */
    public void delCachesBySignature(Account account) {
        String signature = account.getSignature();
        database.delete(FILECACHE_TABLE_NAME, FILECACHE_COLUMN_ACCOUNT + "=?", new String[]{signature});
    }

    public List<SeafCachedFile> getFileCacheItems(DataManager dataManager) {
        List<SeafCachedFile> files = Lists.newArrayList();

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

        if (!cursor.moveToFirst()) {
            cursor.close();
            return null;
        }

        String dir = cursor.getString(0);
        cursor.close();

        return dir;
    }

    public String getCachedStarredFiles(Account account) {
        String[] projection = {
                STARRED_FILECACHE_COLUMN_CONTENT
        };

        String selectClause = String.format("%s = ?",
                STARRED_FILECACHE_COLUMN_ACCOUNT);

        String[] selectArgs = { account.getSignature() };


        Cursor cursor = database.query(
                STARRED_FILECACHE_TABLE_NAME,
                projection,
                selectClause,
                selectArgs,
                null,   // don't group the rows
                null,   // don't filter by row groups
                null);  // The sort order

        if (!cursor.moveToFirst()) {
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

        boolean exist = true;
        if (!cursor.moveToFirst()) {
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

    public void saveCachedStarredFiles(Account account, String content) {
        removeStarredFiles(account);

        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(STARRED_FILECACHE_COLUMN_ACCOUNT, account.getSignature());
        values.put(STARRED_FILECACHE_COLUMN_CONTENT, content);

        database.insert(STARRED_FILECACHE_TABLE_NAME, null, values);
    }

    public void saveDirents(String repoID, String path, String dirID, String content) {
        removeCachedDirents(repoID, path);
        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(DIRENTS_CACHE_COLUMN_REPO_ID, repoID);
        values.put(DIRENTS_CACHE_COLUMN_PATH, path);
        values.put(DIRENTS_CACHE_COLUMN_DIR_ID, dirID);
        values.put(DIRENTS_CACHE_COLUMN_CONTENT, content);

        // Insert the new row, returning the primary key value of the new row
        database.insert(DIRENTS_CACHE_TABLE_NAME, null, values);
    }

    public void removeCachedDirents(String repoID, String path) {
        String whereClause = String.format("%s = ? and %s = ?",
            DIRENTS_CACHE_COLUMN_REPO_ID, DIRENTS_CACHE_COLUMN_PATH);

        database.delete(DIRENTS_CACHE_TABLE_NAME, whereClause, new String[] { repoID, path });
    }

    private void removeStarredFiles(Account account) {
        String whereClause = String.format("%s = ?",
                STARRED_FILECACHE_COLUMN_ACCOUNT);

        database.delete(STARRED_FILECACHE_TABLE_NAME, whereClause, new String[] { account.getSignature() });
    }

    public String getDirents(String repoID, String path, String dirID) {
        Pair<String, String> ret = getCachedDirents(repoID, path);
        if (ret == null) {
            return null;
        }

        if (dirID != null && !ret.first.equals(dirID)) {
            // cache is out of date
            return null;
        }

        return ret.second;
    }

    public Pair<String, String> getCachedDirents(String repoID, String path) {
        String[] projection = {
            DIRENTS_CACHE_COLUMN_DIR_ID,
            DIRENTS_CACHE_COLUMN_CONTENT
        };

        String selectClause = String.format("%s = ? and %s = ?",
                                            DIRENTS_CACHE_COLUMN_REPO_ID,
                                            DIRENTS_CACHE_COLUMN_PATH);

        String[] selectArgs = { repoID, path };

        Cursor cursor = database.query(
            DIRENTS_CACHE_TABLE_NAME,
            projection,
            selectClause,
            selectArgs,
            null,   // don't group the rows
            null,   // don't filter by row groups
            null);  // The sort order

        if (!cursor.moveToFirst()) {
            cursor.close();
            return null;
        }

        String dirID = cursor.getString(0);
        String content = cursor.getString(1);
        cursor.close();

        return new Pair<String, String>(dirID, content);
    }
}
