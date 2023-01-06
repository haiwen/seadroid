package com.seafile.seadroid2.backupdirectory;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;

import java.io.File;

public class UploadDirectoryDBHelper extends SQLiteOpenHelper {
    // version.
    public static final int DATABASE_VERSION = 1;
    public static final String DATABASE_NAME = "directory.db";

    // UploadDir table
    private static final String AUTO_UPDATE_DIR_TABLE_NAME = "UploadDirInfo";
    private static final String AUTO_UPDATE_DIR_COLUMN_ID = "id";
    private static final String AUTO_UPDATE_DIR_COLUMN_ACCOUNT = "account";
    private static final String AUTO_UPDATE_DIR_COLUMN_REPO_ID = "repo_id";
    private static final String AUTO_UPDATE_DIR_COLUMN_REPO_NAME = "repo_name";
    private static final String AUTO_UPDATE_DIR_COLUMN_PARENT_DIR = "parent_dir";
    private static final String AUTO_UPDATE_DIR_COLUMN_FILE_NAME = "file_name";
    private static final String AUTO_UPDATE_DIR_COLUMN_FILE_PATH = "file_path";
    private static final String AUTO_UPDATE_DIR_COLUMN_FILE_SIZE = "file_size";

    // repo config table
    private static final String REPO_CONFIG_TABLE_NAME = "RepoConfig";
    private static final String REPO_CONFIG_COLUMN_ID = "id";
    private static final String REPO_CONFIG_MAIL = "mail";
    private static final String REPO_CONFIG_REPO_ID = "repo_id";
    private static final String REPO_CONFIG_REPO_NAME = "repo_name";


    // paths config table
    private static final String PATH_CONFIG_TABLE_NAME = "PathConfig";
    private static final String PATH_CONFIG_COLUMN_ID = "id";
    private static final String PATH_CONFIG_MAIL = "mail";
    private static final String PATH_CONFIG_DIR_PATHS = "dirPaths";


    private static final String SQL_CREATE_REPO_CONFIG_TABLE =
            "CREATE TABLE " + REPO_CONFIG_TABLE_NAME + " ("
                    + REPO_CONFIG_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + REPO_CONFIG_MAIL + " TEXT NOT NULL, "
                    + REPO_CONFIG_REPO_ID + " TEXT NOT NULL, "
                    + REPO_CONFIG_REPO_NAME + " TEXT NOT NULL);";

    private static final String SQL_CREATE_PATH_CONFIG_TABLE =
            "CREATE TABLE " + PATH_CONFIG_TABLE_NAME + " ("
                    + PATH_CONFIG_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + PATH_CONFIG_MAIL + " TEXT NOT NULL, "
                    + PATH_CONFIG_DIR_PATHS + " TEXT NOT NULL);";


    private static final String SQL_CREATE_AUTO_UPDATE_DIR_TABLE = "CREATE TABLE "
            + AUTO_UPDATE_DIR_TABLE_NAME
            + " ("
            + AUTO_UPDATE_DIR_COLUMN_ID
            + " INTEGER PRIMARY KEY, "
            + AUTO_UPDATE_DIR_COLUMN_ACCOUNT
            + " TEXT NOT NULL, "
            + AUTO_UPDATE_DIR_COLUMN_REPO_ID
            + " TEXT NOT NULL, "
            + AUTO_UPDATE_DIR_COLUMN_REPO_NAME
            + " TEXT NOT NULL, "
            + AUTO_UPDATE_DIR_COLUMN_PARENT_DIR
            + " TEXT NOT NULL, "
            + AUTO_UPDATE_DIR_COLUMN_FILE_NAME
            + " TEXT NOT NULL, "
            + AUTO_UPDATE_DIR_COLUMN_FILE_PATH
            + " TEXT NOT NULL, "
            + AUTO_UPDATE_DIR_COLUMN_FILE_SIZE
            + " TEXT NOT NULL);";


    private SQLiteDatabase database = null;
    private static UploadDirectoryDBHelper dbHelper = null;

    public static synchronized UploadDirectoryDBHelper getDatabaseHelper() {
        if (dbHelper != null)
            return dbHelper;
        dbHelper = new UploadDirectoryDBHelper(SeadroidApplication.getAppContext());
        dbHelper.database = dbHelper.getWritableDatabase();
        return dbHelper;
    }

    private UploadDirectoryDBHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }


    @Override
    public void onCreate(SQLiteDatabase db) {
        createUploadDirTable(db);
    }

    private void createUploadDirTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_AUTO_UPDATE_DIR_TABLE);
        db.execSQL(SQL_CREATE_REPO_CONFIG_TABLE);
        db.execSQL(SQL_CREATE_PATH_CONFIG_TABLE);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + AUTO_UPDATE_DIR_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + SQL_CREATE_REPO_CONFIG_TABLE + ";");
        db.execSQL("DROP TABLE IF EXISTS " + SQL_CREATE_PATH_CONFIG_TABLE + ";");
        onCreate(db);
    }

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

    public void saveDirUploadInfo(UploadDirInfo info) {
        ContentValues values = new ContentValues();
        values.put(AUTO_UPDATE_DIR_COLUMN_ACCOUNT, info.account.getSignature());
        values.put(AUTO_UPDATE_DIR_COLUMN_REPO_ID, info.repoID);
        values.put(AUTO_UPDATE_DIR_COLUMN_REPO_NAME, info.repoName);
        values.put(AUTO_UPDATE_DIR_COLUMN_PARENT_DIR, info.parentDir);
        values.put(AUTO_UPDATE_DIR_COLUMN_FILE_NAME, info.fileName);
        values.put(AUTO_UPDATE_DIR_COLUMN_FILE_PATH, info.filePath);
        values.put(AUTO_UPDATE_DIR_COLUMN_FILE_SIZE, info.fileSize);

        database.insert(AUTO_UPDATE_DIR_TABLE_NAME, null, values);
    }
    public void saveRepoConfig(String email, String repoID, String repoName) {
        ContentValues values = new ContentValues();
        values.put(REPO_CONFIG_MAIL, email);
        values.put(REPO_CONFIG_REPO_ID, repoID);
        values.put(REPO_CONFIG_REPO_NAME, repoName);
        database.insert(REPO_CONFIG_TABLE_NAME, null, values);
    }
    public void savePathsConfig(String email, String filePaths) {
        ContentValues values = new ContentValues();
        values.put(PATH_CONFIG_MAIL, email);
        values.put(PATH_CONFIG_DIR_PATHS, filePaths);
        database.insert(PATH_CONFIG_TABLE_NAME, null, values);
    }

    public void updateRepoConfig(String email, String repoID, String repoName) {
        removeRepoConfig(email);
        saveRepoConfig(email, repoID, repoName);
    }
    public void updatePathsConfig(String email, String filePaths) {
        removePathsConfig(email);
        savePathsConfig(email,filePaths);
    }
    public RepoInfo getRepoConfig(String email) {
        String[] projection = {
                REPO_CONFIG_REPO_ID,
                REPO_CONFIG_REPO_NAME
        };
        String selectClause = String.format("%s = ? ",
                REPO_CONFIG_MAIL);
        String[] selectArgs = {email};


        Cursor c = database.query(
                REPO_CONFIG_TABLE_NAME,
                projection,
                selectClause,
                selectArgs,
                null,
                null,
                null);
        if (!c.moveToFirst()) {
            c.close();
            return null;
        }
        RepoInfo item = cursorToRepoConfigInfo(c,email);
        c.close();
        return item;
    }
    public PathsInfo getPathsConfig(String email) {
        String[] projection = {
                PATH_CONFIG_DIR_PATHS
        };
        String selectClause = String.format("%s = ? ",
                PATH_CONFIG_MAIL);
        String[] selectArgs = {email};


        Cursor c = database.query(
                PATH_CONFIG_TABLE_NAME,
                projection,
                selectClause,
                selectArgs,
                null,
                null,
                null);
        if (!c.moveToFirst()) {
            c.close();
            return null;
        }
        PathsInfo item = cursorToPathsConfigInfo(c,email);
        c.close();
        return item;
    }

    public UploadDirInfo getUploadFileInfo(Account account, String repoID, String filePath, String fileSize) {
        String[] projection = {
                AUTO_UPDATE_DIR_COLUMN_REPO_ID,
                AUTO_UPDATE_DIR_COLUMN_REPO_NAME,
                AUTO_UPDATE_DIR_COLUMN_PARENT_DIR,
                AUTO_UPDATE_DIR_COLUMN_FILE_NAME,
                AUTO_UPDATE_DIR_COLUMN_FILE_PATH,
                AUTO_UPDATE_DIR_COLUMN_FILE_SIZE
        };
        String selectClause = String.format("%s = ? and %s = ? and %s = ? and %s = ?",
                AUTO_UPDATE_DIR_COLUMN_ACCOUNT,
                AUTO_UPDATE_DIR_COLUMN_REPO_ID,
                AUTO_UPDATE_DIR_COLUMN_FILE_PATH,
                AUTO_UPDATE_DIR_COLUMN_FILE_SIZE);
        String[] selectArgs = {account.getSignature(), repoID, filePath, fileSize};


        Cursor c = database.query(
                AUTO_UPDATE_DIR_TABLE_NAME,
                projection,
                selectClause,
                selectArgs,
                null,
                null,
                null);
        if (!c.moveToFirst()) {
            c.close();
            return null;
        }
        UploadDirInfo item = cursorToDirUpdateInfo(c, account);
        c.close();
        return item;
    }

    public void removeRepoConfig(String email) {
        String whereClause = String.format("%s = ?", REPO_CONFIG_MAIL);
        String[] params = {email};
        database.delete(REPO_CONFIG_TABLE_NAME, whereClause, params);
    }

    public void removePathsConfig(String email) {
        String whereClause = String.format("%s = ?", PATH_CONFIG_MAIL);
        database.delete(PATH_CONFIG_TABLE_NAME, whereClause, new String[]{email});
    }


    private UploadDirInfo cursorToDirUpdateInfo(Cursor c, Account account) {
        String repoID = c.getString(0);
        String repoName = c.getString(1);
        String parentDir = c.getString(2);
        String fileName = c.getString(3);
        String filePath = c.getString(4);
        String fileSize = c.getString(5);

        if (!new File(filePath).exists()) {
            filePath = null;
        }

        UploadDirInfo info = new UploadDirInfo(account, repoID, repoName, parentDir, fileName, filePath, fileSize);
        return info;
    }
    private RepoInfo cursorToRepoConfigInfo(Cursor c,String email) {
        String repoID = c.getString(0);
        String repoName = c.getString(1);
        RepoInfo info = new RepoInfo(repoID, repoName, email);
        return info;
    }
    private PathsInfo cursorToPathsConfigInfo(Cursor c, String email) {
        String filePaths = c.getString(0);
        PathsInfo info = new PathsInfo(filePaths,email);
        return info;
    }
}
