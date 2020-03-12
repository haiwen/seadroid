package com.seafile.seadroid2.ssl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.security.cert.X509Certificate;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Base64;

import com.seafile.seadroid2.SeadroidApplication;

public class CertsDBHelper extends SQLiteOpenHelper {
    // If you change the database schema, you must increment the database version.
    private static final int DATABASE_VERSION = 1;
    private static final String DATABASE_NAME = "certs.db";

    private static final String TABLE_NAME = "Certs";

    private static final String COLUMN_URL = "url";
    private static final String COLUMN_CERT = "cert";

    private static final String CREATE_TABLE_SQL = "CREATE TABLE " + TABLE_NAME + " ("
            + COLUMN_URL + " VARCHAR(255) PRIMARY KEY, " + COLUMN_CERT + " TEXT " + ")";

    private static CertsDBHelper dbHelper = null;
    private SQLiteDatabase database = null;

    public static synchronized CertsDBHelper getDatabaseHelper() {
        if (dbHelper != null)
            return dbHelper;
        dbHelper = new CertsDBHelper(SeadroidApplication.getAppContext());
        dbHelper.database = dbHelper.getWritableDatabase();
        return dbHelper;
    }

    private CertsDBHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        db.execSQL(CREATE_TABLE_SQL);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {}

    @Override
    public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {}

    public X509Certificate getCertificate(String url) {
        String[] projection = {COLUMN_CERT};

        Cursor c = database.query(TABLE_NAME,
                                  projection,
                                  "url=?",
                                  new String[] {url},
                                  null,  // don't group the rows
                                  null,  // don't filter by row groups
                                  null); // The sort order

        if (!c.moveToFirst()) {
            c.close();
            return null;
        }

        X509Certificate cert = cursorToCert(c);

        c.close();
        return cert;
    }

    private X509Certificate cursorToCert(Cursor cursor) {
        X509Certificate cert = null;
        String text = cursor.getString(0);

        ByteArrayInputStream bis = null;
        ObjectInputStream ois = null;
        byte[] data = null;

        data = Base64.decode(text, Base64.DEFAULT);

        try {
            bis = new ByteArrayInputStream(data);
            ois = new ObjectInputStream(bis);
            cert = (X509Certificate) ois.readObject();
            return cert;
        } catch (ClassNotFoundException e) {
            return null;
        } catch (IOException e) {
            return null;
        } finally {
            if (bis != null) {
                try {
                    bis.close();
                } catch (IOException e) {
                }
            }

            if (ois != null) {
                try {
                    ois.close();
                } catch (IOException e) {
                }
            }
        }
    }

    public void saveCertificate(String url, X509Certificate cert) {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ObjectOutputStream out = null;
        String text = null;
        try {
            out = new ObjectOutputStream(bos);
            out.writeObject(cert);
            byte[] data = bos.toByteArray();
            text = Base64.encodeToString(data, Base64.DEFAULT);
        } catch (IOException e) {
            return;
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException ex) {
                // ignore close exception
            }
            try {
                bos.close();
            } catch (IOException ex) {
                // ignore close exception
            }
        }

        ContentValues values = new ContentValues();
        values.put(COLUMN_URL, url);
        values.put(COLUMN_CERT, text);

        database.replace(TABLE_NAME, null, values);
    }
}
