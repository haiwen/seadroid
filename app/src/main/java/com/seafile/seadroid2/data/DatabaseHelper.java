package com.seafile.seadroid2.data;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.support.annotation.NonNull;
import android.text.TextUtils;
import android.util.Log;
import android.util.Pair;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;

import java.io.File;
import java.util.List;

public class DatabaseHelper extends SQLiteOpenHelper {
    private static final String DEBUG_TAG = "DatabaseHelper";
    // If you change the database schema, you must increment the database version.
    public static final int DATABASE_VERSION = 9;
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

    /** Table to lookup the mapping from repository to local cache directory.
     * As there can be multiple repositories with the same name (even on the same server)
     * this mapping has to be remembered.
     */
    private static final String REPODIR_TABLE_NAME = "RepoDir";

    private static final String REPODIR_COLUMN_ID = "id";
    /** Signature of the associated account, E.g. "seacloud.cc (user@example.com)" */
    private static final String REPODIR_COLUMN_ACCOUNT = "account";
    /** Repository ID: E.g.: 41deb3fc-192a-4387-8aa1-2020e0727283 */
    private static final String REPODIR_COLUMN_REPO_ID = "repo_id";
    /** Local directory used for cached files, relative to Account cache directory.
     *  E.g.: "Temp Repository (1)" */
    private static final String REPODIR_COLUMN_REPO_DIR = "repo_dir";

    private static final String DIRENTS_CACHE_TABLE_NAME = "DirentsCache";

    private static final String DIRENTS_CACHE_COLUMN_ID = "id";
    private static final String DIRENTS_CACHE_COLUMN_REPO_ID = "repo_id";
    private static final String DIRENTS_CACHE_COLUMN_PATH = "path";
    private static final String DIRENTS_CACHE_COLUMN_DIR_ID = "dir_id";

    public static final String ENCKEY_TABLE_NAME = "EncKey";

    public static final String ENCKEY_COLUMN_ID = "id";
    public static final String ENCKEY_COLUMN_ENCKEY = "enc_key";
    public static final String ENCKEY_COLUMN_ENCIV = "enc_iv";
    public static final String ENCKEY_COLUMN_REPO_ID = "repo_id";

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
        + REPODIR_COLUMN_REPO_ID + " TEXT NOT NULL, "
        + REPODIR_COLUMN_REPO_DIR + " TEXT NOT NULL);";

    private static final String SQL_CREATE_DIRENTS_CACHE_TABLE =
        "CREATE TABLE " + DIRENTS_CACHE_TABLE_NAME + " ("
        + DIRENTS_CACHE_COLUMN_ID + " INTEGER PRIMARY KEY, "
        + DIRENTS_CACHE_COLUMN_REPO_ID + " TEXT NOT NULL, "
        + DIRENTS_CACHE_COLUMN_PATH + " TEXT NOT NULL, "
        + DIRENTS_CACHE_COLUMN_DIR_ID + " TEXT NOT NULL);";

    private static final String SQL_CREATE_ENCKEY_TABLE =
            "CREATE TABLE " + ENCKEY_TABLE_NAME + " ("
                    + ENCKEY_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + ENCKEY_COLUMN_ENCKEY + " TEXT NOT NULL, "
                    + ENCKEY_COLUMN_ENCIV + " TEXT NOT NULL, "
                    + ENCKEY_COLUMN_REPO_ID + " TEXT NOT NULL);";

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
        createEnckeyTable(db);
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

        // index for getRepoDir()
        String sql;
        sql = String.format("CREATE UNIQUE INDEX account_repoid_index ON %s (%s, %s)",
                            REPODIR_TABLE_NAME,
                            REPODIR_COLUMN_ACCOUNT,
                            REPODIR_COLUMN_REPO_ID);
        db.execSQL(sql);

        // index for repoDirExists()
        sql = String.format("CREATE UNIQUE INDEX account_dir_index ON %s (%s, %s)",
                            REPODIR_TABLE_NAME,
                            REPODIR_COLUMN_ACCOUNT,
                            REPODIR_COLUMN_REPO_DIR);
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

    private void createEnckeyTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_ENCKEY_TABLE);

        String sql;
        sql = String.format("CREATE INDEX enckey_repo_index ON %s (%s, %s)",
                ENCKEY_TABLE_NAME,
                ENCKEY_COLUMN_ENCKEY,
                ENCKEY_COLUMN_REPO_ID);
        db.execSQL(sql);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        // This database is only a cache for online data, so its upgrade policy is
        // to simply to discard the data and start over

        File dir = StorageManager.getInstance().getJsonCacheDir();
        for (File f : dir.listFiles()) {
            if (f.isFile()) {
                f.delete();
            }
        }

        db.execSQL("DROP TABLE IF EXISTS " + FILECACHE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + REPODIR_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + DIRENTS_CACHE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + STARRED_FILECACHE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + ENCKEY_TABLE_NAME + ";");
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

    public void delCaches() {
        database.delete(REPODIR_TABLE_NAME, null, null);
        database.delete(FILECACHE_TABLE_NAME, null, null);
        database.delete(DIRENTS_CACHE_TABLE_NAME, null, null);
        database.delete(STARRED_FILECACHE_TABLE_NAME, null, null);
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
    public String getRepoDir(Account account, String repoID) {
        String[] projection = {
            REPODIR_COLUMN_REPO_DIR
        };

        String selectClause = String.format("%s = ? and %s = ?",
                                            REPODIR_COLUMN_ACCOUNT,
                                            REPODIR_COLUMN_REPO_ID);

        String[] selectArgs = { account.getSignature(), repoID };


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
    public boolean repoDirExists(Account account, String dir) {
        String[] projection = {
            REPODIR_COLUMN_REPO_DIR
        };

        String selectClause = String.format("%s = ? and %s = ?",
                                            REPODIR_COLUMN_ACCOUNT,
                                            REPODIR_COLUMN_REPO_DIR);
        String[] selectArgs = { account.getSignature(), dir };

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

    public void saveRepoDirMapping(Account account,
                                   String repoID, String dir) {
        String log = String.format("Saving repo dir mapping: account = %s(%s) "
                        + "repoID = %s"
                        + "dir = %s",
                account.getEmail(), account.getServerNoProtocol(),
                repoID, dir);

        Log.d(DEBUG_TAG, log);

        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(REPODIR_COLUMN_ACCOUNT, account.getSignature());
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

    public void saveDirents(String repoID, String path, String dirID) {
        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(DIRENTS_CACHE_COLUMN_REPO_ID, repoID);
        values.put(DIRENTS_CACHE_COLUMN_PATH, path);
        values.put(DIRENTS_CACHE_COLUMN_DIR_ID, dirID);

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

    public String getCachedDirents(String repoID, String path) {
        String[] projection = {
            DIRENTS_CACHE_COLUMN_DIR_ID
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
        cursor.close();

        return dirID;
    }

    /**
     * Return the number of cached dirs that reference a specific dirID.
     * Used for cache cleaning.
     *
     * @param dirID
     * @return
     */
    public int getCachedDirentUsage(String dirID) {
        String[] projection = { DIRENTS_CACHE_COLUMN_DIR_ID };

        String selectClause = String.format("%s = ?",
                DIRENTS_CACHE_COLUMN_DIR_ID);

        String[] selectArgs = { dirID };

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
            return 0;
        }

        int count = cursor.getCount();
        cursor.close();

        return count;
    }

    public Pair<String, String> getEnckey(@NonNull String repoId) {
        String[] projection = {
                ENCKEY_COLUMN_ENCKEY,
                ENCKEY_COLUMN_ENCIV
        };

        String selectClause = String.format("%s = ?",
                ENCKEY_COLUMN_REPO_ID);

        String [] selectArgs = { repoId };

        Cursor cursor = database.query(
                        ENCKEY_TABLE_NAME,
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

        final String encKey = cursor.getString(0);
        final String encIv = cursor.getString(1);
        cursor.close();

        return new Pair<>(encKey, encIv);
    }

    public void saveEncKey(@NonNull String encKey, @NonNull String encIv, @NonNull String repoId) {
        Pair<String, String> old = getEnckey(repoId);

        if (old != null && !TextUtils.isEmpty(old.first)) {
            if (old.first.equals(encKey) && old.second.equals(encIv)) {
                return;
            } else {
                delEnckey(repoId);
            }
        }

        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(ENCKEY_COLUMN_ENCKEY, encKey);
        values.put(ENCKEY_COLUMN_ENCIV, encIv);
        values.put(ENCKEY_COLUMN_REPO_ID, repoId);

        database.insert(ENCKEY_TABLE_NAME, null, values);
    }

    private void delEnckey(String repoId) {
        database.delete(ENCKEY_TABLE_NAME,  ENCKEY_COLUMN_REPO_ID + "=?",
                new String[] { repoId });
    }

    public void clearEnckeys() {
        database.delete(ENCKEY_TABLE_NAME, null, null);
    }
}
