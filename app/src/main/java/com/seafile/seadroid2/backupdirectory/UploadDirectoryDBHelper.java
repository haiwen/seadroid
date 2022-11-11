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

    // UploadDir config table
    private static final String DIR_CONFIG_INFO_TABLE_NAME = "UploadDirConfig";
    private static final String DIR_CONFIG_INFO_COLUMN_ID = "id";
    private static final String DIR_CONFIG_INFO_ACCOUNT = "account";
    private static final String DIR_CONFIG_INFO_REPO_ID = "repo_id";
    private static final String DIR_CONFIG_INFO_REPO_NAME = "repo_name";
    private static final String DIR_CONFIG_INFO_DIR_NAME = "dir_name";
    private static final String DIR_CONFIG_INFO_DIR_PATH = "dir_path";


    private static final String SQL_CREATE_DIR_CONFIG_TABLE =
            "CREATE TABLE " + DIR_CONFIG_INFO_TABLE_NAME + " ("
                    + DIR_CONFIG_INFO_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + DIR_CONFIG_INFO_ACCOUNT + " TEXT NOT NULL, "
                    + DIR_CONFIG_INFO_REPO_ID + " TEXT NOT NULL, "
                    + DIR_CONFIG_INFO_REPO_NAME + " TEXT NOT NULL, "
                    + DIR_CONFIG_INFO_DIR_NAME + " TEXT NOT NULL, "
                    + DIR_CONFIG_INFO_DIR_PATH + " TEXT NOT NULL);";

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
        db.execSQL(SQL_CREATE_DIR_CONFIG_TABLE);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + AUTO_UPDATE_DIR_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + DIR_CONFIG_INFO_TABLE_NAME + ";");
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
    public void saveDirUploadConfig(Account account, String repoID, String repoName,String fileName,String filePath) {
        ContentValues values = new ContentValues();
        values.put(DIR_CONFIG_INFO_ACCOUNT, account.getSignature());
        values.put(DIR_CONFIG_INFO_REPO_ID, repoID);
        values.put(DIR_CONFIG_INFO_REPO_NAME, repoName);
        values.put(DIR_CONFIG_INFO_DIR_NAME, fileName);
        values.put(DIR_CONFIG_INFO_DIR_PATH, filePath);
        database.insert(DIR_CONFIG_INFO_TABLE_NAME, null, values);
    }

    public void updateDirConfig(Account account,String oldRepoID, String repoID, String repoName,String fileName,String filePath) {
        removeDirConfigInfo(account,oldRepoID,filePath);
        saveDirUploadConfig(account,repoID,repoName,fileName,filePath);
    }
    public UploadDirConfig getDirConfig(Account account, String filePath) {
        String[] projection = {
                DIR_CONFIG_INFO_REPO_ID,
                DIR_CONFIG_INFO_REPO_NAME,
                DIR_CONFIG_INFO_DIR_NAME,
                DIR_CONFIG_INFO_DIR_PATH
        };
        String selectClause = String.format("%s = ? and %s = ? ",
                DIR_CONFIG_INFO_ACCOUNT,
                DIR_CONFIG_INFO_DIR_PATH);
        String[] selectArgs = {account.getSignature(), filePath};


        Cursor c = database.query(
                DIR_CONFIG_INFO_TABLE_NAME,
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
        UploadDirConfig item = cursorToDirConfigInfo(c, account);
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

    public void removeDirUploadInfo(UploadDirInfo info) {
        String whereClause = String.format(
                "%s = ? and %s = ? and %s = ? and %s = ? and %s = ? and %s = ?",
                AUTO_UPDATE_DIR_COLUMN_ACCOUNT,
                AUTO_UPDATE_DIR_COLUMN_REPO_ID,
                AUTO_UPDATE_DIR_COLUMN_REPO_NAME,
                AUTO_UPDATE_DIR_COLUMN_PARENT_DIR,
                AUTO_UPDATE_DIR_COLUMN_FILE_NAME,
                AUTO_UPDATE_DIR_COLUMN_FILE_PATH,
                AUTO_UPDATE_DIR_COLUMN_FILE_SIZE);
        String[] params = {info.account.getSignature(), info.repoID, info.repoName,
                info.parentDir, info.filePath,};
        database.delete(AUTO_UPDATE_DIR_TABLE_NAME, whereClause, params);
    }

    public void removeDirConfigInfo(Account account, String repoID, String filePath) {
        String whereClause = String.format("%s = ? and %s = ? and %s = ?",
                DIR_CONFIG_INFO_ACCOUNT, DIR_CONFIG_INFO_REPO_ID, DIR_CONFIG_INFO_DIR_PATH);

        database.delete(DIR_CONFIG_INFO_TABLE_NAME, whereClause, new String[] { account.getSignature(), repoID, filePath });
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
    private UploadDirConfig cursorToDirConfigInfo(Cursor c, Account account) {
        String repoID = c.getString(0);
        String repoName = c.getString(1);
        String fileName = c.getString(2);
        String filePath = c.getString(3);

        if (!new File(filePath).exists()) {
            filePath = null;
        }

        UploadDirConfig info = new UploadDirConfig(account, repoID, repoName, fileName, filePath);
        return info;
    }
}
