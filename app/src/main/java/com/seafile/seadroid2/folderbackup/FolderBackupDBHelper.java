package com.seafile.seadroid2.folderbackup;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import com.seafile.seadroid2.SeadroidApplication;

import java.io.File;

public class FolderBackupDBHelper extends SQLiteOpenHelper {

    private SQLiteDatabase database = null;
    private static FolderBackupDBHelper dbHelper = null;
    // version.
    public static final int DATABASE_VERSION = 1;
    public static final String DATABASE_NAME = "folder_backup.db";
    // backup file table
    private static final String SAVE_FOLDER_BACKUP_FILE_TABLE_NAME = "FolderBackupInfo";
    private static final String FOLDER_BACKUP_COLUMN_ID = "id";
    private static final String FOLDER_BACKUP_COLUMN_REPO_ID = "repo_id";
    private static final String FOLDER_BACKUP_COLUMN_REPO_NAME = "repo_name";
    private static final String FOLDER_BACKUP_COLUMN_PARENT_FOLDER = "parent_folder";
    private static final String FOLDER_BACKUP_COLUMN_FILE_NAME = "file_name";
    private static final String FOLDER_BACKUP_COLUMN_FILE_PATH = "file_path";
    private static final String FOLDER_BACKUP_COLUMN_FILE_SIZE = "file_size";
    // repo config table
    private static final String REPO_CONFIG_TABLE_NAME = "RepoConfig";
    private static final String REPO_CONFIG_COLUMN_ID = "id";
    private static final String REPO_CONFIG_MAIL = "mail";
    private static final String REPO_CONFIG_REPO_ID = "repo_id";
    private static final String REPO_CONFIG_REPO_NAME = "repo_name";

    private static final String SQL_CREATE_REPO_CONFIG_TABLE =
            "CREATE TABLE " + REPO_CONFIG_TABLE_NAME + " ("
                    + REPO_CONFIG_COLUMN_ID + " INTEGER PRIMARY KEY, "
                    + REPO_CONFIG_MAIL + " TEXT NOT NULL, "
                    + REPO_CONFIG_REPO_ID + " TEXT NOT NULL, "
                    + REPO_CONFIG_REPO_NAME + " TEXT NOT NULL);";

    private static final String SQL_CREATE_AUTO_BACKUP_FOLDER_TABLE = "CREATE TABLE "
            + SAVE_FOLDER_BACKUP_FILE_TABLE_NAME
            + " ("
            + FOLDER_BACKUP_COLUMN_ID
            + " INTEGER PRIMARY KEY, "
            + FOLDER_BACKUP_COLUMN_REPO_ID
            + " TEXT NOT NULL, "
            + FOLDER_BACKUP_COLUMN_REPO_NAME
            + " TEXT NOT NULL, "
            + FOLDER_BACKUP_COLUMN_PARENT_FOLDER
            + " TEXT NOT NULL, "
            + FOLDER_BACKUP_COLUMN_FILE_NAME
            + " TEXT NOT NULL, "
            + FOLDER_BACKUP_COLUMN_FILE_PATH
            + " TEXT NOT NULL, "
            + FOLDER_BACKUP_COLUMN_FILE_SIZE
            + " TEXT NOT NULL);";

    public static synchronized FolderBackupDBHelper getDatabaseHelper() {
        if (dbHelper != null)
            return dbHelper;
        dbHelper = new FolderBackupDBHelper(SeadroidApplication.getAppContext());
        dbHelper.database = dbHelper.getWritableDatabase();
        return dbHelper;
    }

    private FolderBackupDBHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        createFolderBackupTable(db);
    }

    private void createFolderBackupTable(SQLiteDatabase db) {
        db.execSQL(SQL_CREATE_AUTO_BACKUP_FOLDER_TABLE);
        db.execSQL(SQL_CREATE_REPO_CONFIG_TABLE);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL("DROP TABLE IF EXISTS " + SAVE_FOLDER_BACKUP_FILE_TABLE_NAME + ";");
        db.execSQL("DROP TABLE IF EXISTS " + SQL_CREATE_REPO_CONFIG_TABLE + ";");
        onCreate(db);
    }

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }

    public void saveFileBackupInfo(FolderBackupInfo info) {
        ContentValues values = new ContentValues();
        values.put(FOLDER_BACKUP_COLUMN_REPO_ID, info.repoID);
        values.put(FOLDER_BACKUP_COLUMN_REPO_NAME, info.repoName);
        values.put(FOLDER_BACKUP_COLUMN_PARENT_FOLDER, info.parentFolder);
        values.put(FOLDER_BACKUP_COLUMN_FILE_NAME, info.fileName);
        values.put(FOLDER_BACKUP_COLUMN_FILE_PATH, info.filePath);
        values.put(FOLDER_BACKUP_COLUMN_FILE_SIZE, info.fileSize);
        database.insert(SAVE_FOLDER_BACKUP_FILE_TABLE_NAME, null, values);
    }

    public void saveRepoConfig(String email, String repoID, String repoName) {
        ContentValues values = new ContentValues();
        values.put(REPO_CONFIG_MAIL, email);
        values.put(REPO_CONFIG_REPO_ID, repoID);
        values.put(REPO_CONFIG_REPO_NAME, repoName);
        database.insert(REPO_CONFIG_TABLE_NAME, null, values);
    }

    public void updateRepoConfig(String email, String repoID, String repoName) {
        removeRepoConfig(email);
        saveRepoConfig(email, repoID, repoName);
    }

    public RepoConfig getRepoConfig(String email) {
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
        RepoConfig item = cursorToRepoConfigInfo(c, email);
        c.close();
        return item;
    }

    public FolderBackupInfo getBackupFileInfo(String repoID, String filePath, String fileSize) {
        String[] projection = {
                FOLDER_BACKUP_COLUMN_REPO_ID,
                FOLDER_BACKUP_COLUMN_REPO_NAME,
                FOLDER_BACKUP_COLUMN_PARENT_FOLDER,
                FOLDER_BACKUP_COLUMN_FILE_NAME,
                FOLDER_BACKUP_COLUMN_FILE_PATH,
                FOLDER_BACKUP_COLUMN_FILE_SIZE
        };
        String selectClause = String.format("%s = ? and %s = ? and %s = ?",
                FOLDER_BACKUP_COLUMN_REPO_ID,
                FOLDER_BACKUP_COLUMN_FILE_PATH,
                FOLDER_BACKUP_COLUMN_FILE_SIZE);
        String[] selectArgs = {repoID, filePath, fileSize};
        Cursor c = database.query(
                SAVE_FOLDER_BACKUP_FILE_TABLE_NAME,
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
        FolderBackupInfo item = cursorToBackupUpdateInfo(c);
        c.close();
        return item;
    }

    public void removeRepoConfig(String email) {
        String whereClause = String.format("%s = ?", REPO_CONFIG_MAIL);
        String[] params = {email};
        database.delete(REPO_CONFIG_TABLE_NAME, whereClause, params);
    }

    private FolderBackupInfo cursorToBackupUpdateInfo(Cursor c) {
        String repoID = c.getString(0);
        String repoName = c.getString(1);
        String parentDir = c.getString(2);
        String fileName = c.getString(3);
        String filePath = c.getString(4);
        String fileSize = c.getString(5);

        if (!new File(filePath).exists()) {
            filePath = null;
        }

        return new FolderBackupInfo(
                repoID, repoName, parentDir,
                fileName, filePath, fileSize);
    }

    private RepoConfig cursorToRepoConfigInfo(Cursor c, String email) {
        String repoID = c.getString(0);
        String repoName = c.getString(1);
        return new RepoConfig(repoID, repoName, email);
    }
}
