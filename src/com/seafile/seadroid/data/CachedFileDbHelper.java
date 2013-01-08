package com.seafile.seadroid.data;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

public class CachedFileDbHelper extends SQLiteOpenHelper {
    // If you change the database schema, you must increment the database version.
    public static final int DATABASE_VERSION = 2;
    public static final String DATABASE_NAME = "data.db";

    public static final String TABLE_NAME = "FileCache";
    
    public static final String COLUMN_ID = "id";
    public static final String COLUMN_FILEID = "fileid";
    public static final String COLUMN_REPO = "repo";
    public static final String COLUMN_PATH = "path";
    public static final String COLUMN_CTIME = "ctime";

    public CachedFileDbHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }
    
    public void onCreate(SQLiteDatabase db) {
        String create = "CREATE TABLE " + TABLE_NAME + " ("
                + COLUMN_ID + " INTEGER PRIMARY KEY, " 
                + COLUMN_FILEID + " TEXT, " 
                + COLUMN_PATH + " TEXT, " + COLUMN_REPO + " TEXT, "
                + COLUMN_CTIME + " INTEGER);";
        db.execSQL(create);
        db.execSQL("CREATE INDEX fileid_index ON " + TABLE_NAME
                + " (" + COLUMN_FILEID + ");");
        db.execSQL("CREATE INDEX repoid_index ON " + TABLE_NAME
                + " (" + COLUMN_REPO + ");");
    }
    
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        // This database is only a cache for online data, so its upgrade policy is
        // to simply to discard the data and start over
        db.execSQL("DROP TABLE IF EXISTS " + TABLE_NAME + ";"); 
        onCreate(db);
    }
    
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        onUpgrade(db, oldVersion, newVersion);
    }
    
    public SeafCachedFile getItem(String repoID, String path) {
        SQLiteDatabase db = getReadableDatabase();

        String[] projection = {   
                COLUMN_ID,
                COLUMN_FILEID,
                COLUMN_REPO,
                COLUMN_PATH,
                COLUMN_CTIME
        };

        Cursor c = db.query(
             TABLE_NAME,
             projection,
             COLUMN_REPO + "=? and " + COLUMN_PATH + "=?",
             new String[] { repoID, path },                                
             null,   // don't group the rows
             null,   // don't filter by row groups
             null    // The sort order
        );
     
        if (c.moveToFirst() == false) {
            c.close();
            db.close();
            return null;
        }

        SeafCachedFile item = cursorToItem(c);
        c.close();
        db.close();
        return item;
    }
    
    public void saveItem(SeafCachedFile item) {
        SeafCachedFile old = getItem(item.repo, item.path);
        if (old != null) {
            if (old.fileID.equals(item.fileID))
                return;
            else
                deleteItem(old);
        }
        
        // Gets the data repository in write mode
        SQLiteDatabase db = getWritableDatabase();
        
        // Create a new map of values, where column names are the keys
        ContentValues values = new ContentValues();
        values.put(COLUMN_FILEID, item.fileID);
        values.put(COLUMN_REPO, item.repo);
        values.put(COLUMN_PATH, item.path);
        values.put(COLUMN_CTIME, item.ctime);

        // Insert the new row, returning the primary key value of the new row
        db.insert(TABLE_NAME, null, values);
        db.close();
    }
    
    public void deleteItem(SeafCachedFile item) {
        // Gets the data repository in write mode
        SQLiteDatabase db = getWritableDatabase();

        if (item.id != -1) {
            db.delete(TABLE_NAME,  COLUMN_ID + "=?",
                    new String[] { String.valueOf(item.id) });
        } else
            db.delete(TABLE_NAME,  COLUMN_REPO + "=? and " + COLUMN_PATH + "=?",
                new String[] { item.repo, item.path });
        db.close();
    }

    public List<SeafCachedFile> getItems() {
        List<SeafCachedFile> files = new ArrayList<SeafCachedFile>();
        
        SQLiteDatabase db = getReadableDatabase();

        String[] projection = {
                COLUMN_ID,
                COLUMN_FILEID,
                COLUMN_REPO,
                COLUMN_PATH,
                COLUMN_CTIME
        };

        Cursor c = db.query(
             TABLE_NAME,
             projection,
             null,
             null,                                     
             null,   // don't group the rows
             null,   // don't filter by row groups
             null    // The sort order
        );
        
        c.moveToFirst();
        while (!c.isAfterLast()) {
            SeafCachedFile item = cursorToItem(c);
            files.add(item);
            c.moveToNext();
        }
        
        c.close();
        db.close();
        return files;
    }
    
    private SeafCachedFile cursorToItem(Cursor cursor) {
        SeafCachedFile item = new SeafCachedFile();
        item.id = cursor.getInt(0);
        item.fileID = cursor.getString(1);
        item.repo = cursor.getString(2);
        item.path = cursor.getString(3);
        item.ctime = cursor.getLong(4);
        item.file = DataManager.getFileForFileCache(item.path, item.fileID);
        return item;
    }

}
