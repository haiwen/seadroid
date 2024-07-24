package com.seafile.seadroid2.ssl;

import android.database.Cursor;
import android.util.Base64;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.framework.data.db.AppDatabase;
import com.seafile.seadroid2.framework.data.db.entities.CertEntity;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.security.cert.X509Certificate;
import java.util.List;

public class CertsHelper {

    public static X509Certificate getCertificate(String url) {
        List<CertEntity> list = AppDatabase.getInstance().certDAO().getListByUrl(url);
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }

        return convertToCert(list.get(0).cert);

//        String[] projection = {COLUMN_CERT};
//
//        Cursor c = database.query(TABLE_NAME,
//                projection,
//                "url=?",
//                new String[]{url},
//                null,  // don't group the rows
//                null,  // don't filter by row groups
//                null); // The sort order
//
//        if (!c.moveToFirst()) {
//            c.close();
//            return null;
//        }
//
//        X509Certificate cert = cursorToCert(c);
//
//        c.close();
//        return cert;
    }

    private static X509Certificate convertToCert(String text) {
        X509Certificate cert = null;

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

    private static X509Certificate cursorToCert(Cursor cursor) {
        X509Certificate cert = null;
        String text = cursor.getString(0);
        return convertToCert(text);
    }

    public static void saveCertificate(String url, X509Certificate cert) {
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

//        ContentValues values = new ContentValues();
//        values.put(COLUMN_URL, url);
//        values.put(COLUMN_CERT, text);
//        database.replace(TABLE_NAME, null, values);

        CertEntity certEntity = new CertEntity();
        certEntity.cert = text;
        certEntity.url = url;

        AppDatabase.getInstance().certDAO().deleteByUrl(url);
        AppDatabase.getInstance().certDAO().insert(certEntity);

    }
}
