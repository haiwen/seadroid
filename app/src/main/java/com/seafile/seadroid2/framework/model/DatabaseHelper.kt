package com.seafile.seadroid2.framework.model

import android.content.ContentValues
import android.content.Context
import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import android.database.sqlite.SQLiteOpenHelper
import android.text.TextUtils
import android.util.Log
import android.util.Pair
import androidx.annotation.NonNull
import com.seafile.seadroid2.SeadroidApplication
import com.seafile.seadroid2.account.Account
import com.seafile.seadroid2.framework.datastore.StorageManager
import java.io.File

class DatabaseHelper private constructor(context: Context) :
    SQLiteOpenHelper(context, DATABASE_NAME, null, DATABASE_VERSION) {

    private var database: SQLiteDatabase = writableDatabase

    override fun onCreate(db: SQLiteDatabase) {
        createFileCacheTable(db)
        createRepoDirTable(db)
        createDirentsCacheTable(db)
        createStarredFilesCacheTable(db)
        createEnckeyTable(db)
    }

    private fun createFileCacheTable(db: SQLiteDatabase) {
        db.execSQL(SQL_CREATE_FILECACHE_TABLE)
        db.execSQL("CREATE INDEX fileid_index ON $FILECACHE_TABLE_NAME ($FILECACHE_COLUMN_FILEID);")
        db.execSQL("CREATE INDEX repoid_index ON $FILECACHE_TABLE_NAME ($FILECACHE_COLUMN_REPO_ID);")
        db.execSQL("CREATE INDEX account_index ON $FILECACHE_TABLE_NAME ($FILECACHE_COLUMN_ACCOUNT);")
    }

    private fun createRepoDirTable(db: SQLiteDatabase) {
        db.execSQL(SQL_CREATE_REPODIR_TABLE)
        var sql = String.format(
            "CREATE UNIQUE INDEX account_repoid_index ON %s (%s, %s)",
            REPODIR_TABLE_NAME,
            REPODIR_COLUMN_ACCOUNT,
            REPODIR_COLUMN_REPO_ID
        )
        db.execSQL(sql)

        sql = String.format(
            "CREATE UNIQUE INDEX account_dir_index ON %s (%s, %s)",
            REPODIR_TABLE_NAME,
            REPODIR_COLUMN_ACCOUNT,
            REPODIR_COLUMN_REPO_DIR
        )
        db.execSQL(sql)
    }

    private fun createDirentsCacheTable(db: SQLiteDatabase) {
        db.execSQL(SQL_CREATE_DIRENTS_CACHE_TABLE)
        val sql = String.format(
            "CREATE INDEX repo_path_index ON %s (%s, %s)",
            DIRENTS_CACHE_TABLE_NAME,
            DIRENTS_CACHE_COLUMN_REPO_ID,
            DIRENTS_CACHE_COLUMN_PATH
        )
        db.execSQL(sql)
    }

    private fun createStarredFilesCacheTable(db: SQLiteDatabase) {
        db.execSQL(SQL_CREATE_STARRED_FILECACHE_TABLE)
        val sql = String.format(
            "CREATE INDEX account_content_index ON %s (%s, %s)",
            STARRED_FILECACHE_TABLE_NAME,
            STARRED_FILECACHE_COLUMN_ACCOUNT,
            STARRED_FILECACHE_COLUMN_CONTENT
        )
        db.execSQL(sql)
    }

    private fun createEnckeyTable(db: SQLiteDatabase) {
        db.execSQL(SQL_CREATE_ENCKEY_TABLE)
        val sql = String.format(
            "CREATE INDEX enckey_repo_index ON %s (%s, %s)",
            ENCKEY_TABLE_NAME,
            ENCKEY_COLUMN_ENCKEY,
            ENCKEY_COLUMN_REPO_ID
        )
        db.execSQL(sql)
    }

    override fun onUpgrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int) {
        val dir: File = StorageManager.getInstance().jsonCacheDir
        dir.listFiles()?.filter { it.isFile }?.forEach { it.delete() }

        db.execSQL("DROP TABLE IF EXISTS $FILECACHE_TABLE_NAME;")
        db.execSQL("DROP TABLE IF EXISTS $REPODIR_TABLE_NAME;")
        db.execSQL("DROP TABLE IF EXISTS $DIRENTS_CACHE_TABLE_NAME;")
        db.execSQL("DROP TABLE IF EXISTS $STARRED_FILECACHE_TABLE_NAME;")
        db.execSQL("DROP TABLE IF EXISTS $ENCKEY_TABLE_NAME;")
        onCreate(db)
    }

    override fun onDowngrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int) {
        onUpgrade(db, oldVersion, newVersion)
    }

    private fun requireDatabase(): SQLiteDatabase {
        if (!database.isOpen) {
            database = writableDatabase
        }
        return database
    }

    fun delCaches() {
        val db = requireDatabase()
        db.delete(REPODIR_TABLE_NAME, null, null)
        db.delete(FILECACHE_TABLE_NAME, null, null)
        db.delete(DIRENTS_CACHE_TABLE_NAME, null, null)
        db.delete(STARRED_FILECACHE_TABLE_NAME, null, null)
    }

    fun getRepoDir(account: Account, repoID: String): String? {
        val projection = arrayOf(REPODIR_COLUMN_REPO_DIR)
        val selectClause = String.format(
            "%s = ? and %s = ?",
            REPODIR_COLUMN_ACCOUNT,
            REPODIR_COLUMN_REPO_ID
        )
        val selectArgs = arrayOf(account.signature, repoID)

        return querySingleString(
            table = REPODIR_TABLE_NAME,
            projection = projection,
            selection = selectClause,
            selectionArgs = selectArgs
        )
    }

    fun getCachedStarredFiles(account: Account): String? {
        val projection = arrayOf(STARRED_FILECACHE_COLUMN_CONTENT)
        val selectClause = String.format("%s = ?", STARRED_FILECACHE_COLUMN_ACCOUNT)
        val selectArgs = arrayOf(account.signature)

        return querySingleString(
            table = STARRED_FILECACHE_TABLE_NAME,
            projection = projection,
            selection = selectClause,
            selectionArgs = selectArgs
        )
    }

    fun repoDirExists(account: Account, dir: String): Boolean {
        val projection = arrayOf(REPODIR_COLUMN_REPO_DIR)
        val selectClause = String.format(
            "%s = ? and %s = ?",
            REPODIR_COLUMN_ACCOUNT,
            REPODIR_COLUMN_REPO_DIR
        )
        val selectArgs = arrayOf(account.signature, dir)

        val db = requireDatabase()
        db.query(
            REPODIR_TABLE_NAME,
            projection,
            selectClause,
            selectArgs,
            null,
            null,
            null
        ).use { cursor ->
            return cursor.moveToFirst()
        }
    }

    fun saveRepoDirMapping(account: Account, repoID: String, dir: String) {
        val log = String.format(
            "Saving repo dir mapping: account = %s(%s) repoID = %sdir = %s",
            account.email,
            account.serverNoProtocol,
            repoID,
            dir
        )
        Log.d(DEBUG_TAG, log)

        val values = ContentValues().apply {
            put(REPODIR_COLUMN_ACCOUNT, account.signature)
            put(REPODIR_COLUMN_REPO_ID, repoID)
            put(REPODIR_COLUMN_REPO_DIR, dir)
        }
        requireDatabase().insert(REPODIR_TABLE_NAME, null, values)
    }

    fun saveCachedStarredFiles(account: Account, content: String) {
        removeStarredFiles(account)
        val values = ContentValues().apply {
            put(STARRED_FILECACHE_COLUMN_ACCOUNT, account.signature)
            put(STARRED_FILECACHE_COLUMN_CONTENT, content)
        }
        requireDatabase().insert(STARRED_FILECACHE_TABLE_NAME, null, values)
    }

    fun saveDirents(repoID: String, path: String, dirID: String) {
        val values = ContentValues().apply {
            put(DIRENTS_CACHE_COLUMN_REPO_ID, repoID)
            put(DIRENTS_CACHE_COLUMN_PATH, path)
            put(DIRENTS_CACHE_COLUMN_DIR_ID, dirID)
        }
        requireDatabase().insert(DIRENTS_CACHE_TABLE_NAME, null, values)
    }

    fun removeCachedDirents(repoID: String, path: String) {
        val whereClause = String.format(
            "%s = ? and %s = ?",
            DIRENTS_CACHE_COLUMN_REPO_ID,
            DIRENTS_CACHE_COLUMN_PATH
        )
        requireDatabase().delete(
            DIRENTS_CACHE_TABLE_NAME,
            whereClause,
            arrayOf(repoID, path)
        )
    }

    private fun removeStarredFiles(account: Account) {
        val whereClause = String.format("%s = ?", STARRED_FILECACHE_COLUMN_ACCOUNT)
        requireDatabase().delete(
            STARRED_FILECACHE_TABLE_NAME,
            whereClause,
            arrayOf(account.signature)
        )
    }

    fun getCachedDirents(repoID: String, path: String): String? {
        val projection = arrayOf(DIRENTS_CACHE_COLUMN_DIR_ID)
        val selectClause = String.format(
            "%s = ? and %s = ?",
            DIRENTS_CACHE_COLUMN_REPO_ID,
            DIRENTS_CACHE_COLUMN_PATH
        )
        val selectArgs = arrayOf(repoID, path)

        return querySingleString(
            table = DIRENTS_CACHE_TABLE_NAME,
            projection = projection,
            selection = selectClause,
            selectionArgs = selectArgs
        )
    }

    fun getCachedDirentUsage(dirID: String): Int {
        val projection = arrayOf(DIRENTS_CACHE_COLUMN_DIR_ID)
        val selectClause = String.format("%s = ?", DIRENTS_CACHE_COLUMN_DIR_ID)
        val selectArgs = arrayOf(dirID)
        val db = requireDatabase()

        db.query(
            DIRENTS_CACHE_TABLE_NAME,
            projection,
            selectClause,
            selectArgs,
            null,
            null,
            null
        ).use { cursor ->
            if (!cursor.moveToFirst()) {
                return 0
            }
            return cursor.count
        }
    }

    fun getEnckey(@NonNull repoId: String): Pair<String, String>? {
        val projection = arrayOf(ENCKEY_COLUMN_ENCKEY, ENCKEY_COLUMN_ENCIV)
        val selectClause = String.format("%s = ?", ENCKEY_COLUMN_REPO_ID)
        val selectArgs = arrayOf(repoId)
        val db = requireDatabase()

        db.query(
            ENCKEY_TABLE_NAME,
            projection,
            selectClause,
            selectArgs,
            null,
            null,
            null
        ).use { cursor ->
            if (!cursor.moveToFirst()) {
                return null
            }
            val encKey = cursor.getString(0)
            val encIv = cursor.getString(1)
            return Pair(encKey, encIv)
        }
    }

    fun saveEncKey(@NonNull encKey: String, @NonNull encIv: String, @NonNull repoId: String) {
        val old = getEnckey(repoId)
        if (old != null && !TextUtils.isEmpty(old.first)) {
            if (old.first == encKey && old.second == encIv) {
                return
            } else {
                delEnckey(repoId)
            }
        }

        val values = ContentValues().apply {
            put(ENCKEY_COLUMN_ENCKEY, encKey)
            put(ENCKEY_COLUMN_ENCIV, encIv)
            put(ENCKEY_COLUMN_REPO_ID, repoId)
        }
        requireDatabase().insert(ENCKEY_TABLE_NAME, null, values)
    }

    private fun delEnckey(repoId: String) {
        requireDatabase().delete(
            ENCKEY_TABLE_NAME,
            "$ENCKEY_COLUMN_REPO_ID=?",
            arrayOf(repoId)
        )
    }

    fun clearEnckeys() {
        requireDatabase().delete(ENCKEY_TABLE_NAME, null, null)
    }

    private fun querySingleString(
        table: String,
        projection: Array<String>,
        selection: String,
        selectionArgs: Array<String>
    ): String? {
        val db = requireDatabase()
        db.query(
            table,
            projection,
            selection,
            selectionArgs,
            null,
            null,
            null
        ).use { cursor ->
            if (!cursor.moveToFirst()) {
                return null
            }
            return cursor.getString(0)
        }
    }

    companion object {
        private const val DEBUG_TAG = "DatabaseHelper"
        const val DATABASE_VERSION = 9
        const val DATABASE_NAME = "data.db"

        private const val FILECACHE_TABLE_NAME = "FileCache"
        private const val FILECACHE_COLUMN_ID = "id"
        private const val FILECACHE_COLUMN_FILEID = "fileid"
        private const val FILECACHE_COLUMN_REPO_NAME = "repo_name"
        private const val FILECACHE_COLUMN_REPO_ID = "repo_id"
        private const val FILECACHE_COLUMN_PATH = "path"
        private const val FILECACHE_COLUMN_ACCOUNT = "account"

        private const val STARRED_FILECACHE_TABLE_NAME = "StarredFileCache"
        private const val STARRED_FILECACHE_COLUMN_ID = "id"
        private const val STARRED_FILECACHE_COLUMN_ACCOUNT = "account"
        private const val STARRED_FILECACHE_COLUMN_CONTENT = "content"

        private const val REPODIR_TABLE_NAME = "RepoDir"
        private const val REPODIR_COLUMN_ID = "id"
        private const val REPODIR_COLUMN_ACCOUNT = "account"
        private const val REPODIR_COLUMN_REPO_ID = "repo_id"
        private const val REPODIR_COLUMN_REPO_DIR = "repo_dir"

        private const val DIRENTS_CACHE_TABLE_NAME = "DirentsCache"
        private const val DIRENTS_CACHE_COLUMN_ID = "id"
        private const val DIRENTS_CACHE_COLUMN_REPO_ID = "repo_id"
        private const val DIRENTS_CACHE_COLUMN_PATH = "path"
        private const val DIRENTS_CACHE_COLUMN_DIR_ID = "dir_id"

        const val ENCKEY_TABLE_NAME = "EncKey"
        const val ENCKEY_COLUMN_ID = "id"
        const val ENCKEY_COLUMN_ENCKEY = "enc_key"
        const val ENCKEY_COLUMN_ENCIV = "enc_iv"
        const val ENCKEY_COLUMN_REPO_ID = "repo_id"

        private const val SQL_CREATE_FILECACHE_TABLE =
            "CREATE TABLE $FILECACHE_TABLE_NAME (" +
                "$FILECACHE_COLUMN_ID INTEGER PRIMARY KEY, " +
                "$FILECACHE_COLUMN_FILEID TEXT NOT NULL, " +
                "$FILECACHE_COLUMN_PATH TEXT NOT NULL, " +
                "$FILECACHE_COLUMN_REPO_NAME TEXT NOT NULL, " +
                "$FILECACHE_COLUMN_REPO_ID TEXT NOT NULL, " +
                "$FILECACHE_COLUMN_ACCOUNT TEXT NOT NULL);"

        private const val SQL_CREATE_STARRED_FILECACHE_TABLE =
            "CREATE TABLE $STARRED_FILECACHE_TABLE_NAME (" +
                "$STARRED_FILECACHE_COLUMN_ID INTEGER PRIMARY KEY, " +
                "$STARRED_FILECACHE_COLUMN_ACCOUNT TEXT NOT NULL, " +
                "$STARRED_FILECACHE_COLUMN_CONTENT TEXT NOT NULL);"

        private const val SQL_CREATE_REPODIR_TABLE =
            "CREATE TABLE $REPODIR_TABLE_NAME (" +
                "$REPODIR_COLUMN_ID INTEGER PRIMARY KEY, " +
                "$REPODIR_COLUMN_ACCOUNT TEXT NOT NULL, " +
                "$REPODIR_COLUMN_REPO_ID TEXT NOT NULL, " +
                "$REPODIR_COLUMN_REPO_DIR TEXT NOT NULL);"

        private const val SQL_CREATE_DIRENTS_CACHE_TABLE =
            "CREATE TABLE $DIRENTS_CACHE_TABLE_NAME (" +
                "$DIRENTS_CACHE_COLUMN_ID INTEGER PRIMARY KEY, " +
                "$DIRENTS_CACHE_COLUMN_REPO_ID TEXT NOT NULL, " +
                "$DIRENTS_CACHE_COLUMN_PATH TEXT NOT NULL, " +
                "$DIRENTS_CACHE_COLUMN_DIR_ID TEXT NOT NULL);"

        private const val SQL_CREATE_ENCKEY_TABLE =
            "CREATE TABLE $ENCKEY_TABLE_NAME (" +
                "$ENCKEY_COLUMN_ID INTEGER PRIMARY KEY, " +
                "$ENCKEY_COLUMN_ENCKEY TEXT NOT NULL, " +
                "$ENCKEY_COLUMN_ENCIV TEXT NOT NULL, " +
                "$ENCKEY_COLUMN_REPO_ID TEXT NOT NULL);"

        @Volatile
        private var dbHelper: DatabaseHelper? = null

        @JvmStatic
        @Synchronized
        fun getDatabaseHelper(): DatabaseHelper {
            var helper = dbHelper
            if (helper == null) {
                helper = DatabaseHelper(SeadroidApplication.getAppContext())
                helper.database = helper.writableDatabase
                dbHelper = helper
            } else if (!helper.database.isOpen) {
                helper.database = helper.writableDatabase
            }
            return helper
        }
    }
}
